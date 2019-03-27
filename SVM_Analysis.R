####################################################################################
###part 1: initial analysis#########################################################

#install.packages(c("tidyquant", "lubridate", "glue", "readr", "quantmod", "data.table", "rvest", "e1071", "xts", "fPortfolio"))
#install.packages("dplyr")

library(dplyr)
library(tidyquant)
library(lubridate)
library(glue)
library(readr)

library(data.table)
library(rvest)
library(e1071)
library(xts)
library(fPortfolio)

sp_400 <- read_csv("sp-400-index-10-10-2018.csv")
sp_500 <- read_csv("sp-500-index-10-10-2018.csv")
sp_600 <- read_csv("sp-600-index-10-10-2018.csv")
stock_list <- c(sp_400$Symbol, sp_500$Symbol, sp_600$Symbol)


#number of months should be 74
df = data.frame(matrix(0, ncol = 0, nrow = 74))

stock_error <- character()

get_stock <- function(stock){
  stock_data <- getSymbols(stock, src="yahoo", 
                           from = from.dat, to = to.dat, periodicity="monthly", auto.assign = FALSE) %>% data.frame() 
  return(stock_data)
}


#our initial analysis is to the end of 2018, to check the data exists
#when we calculate the beta or machine learning, we only use data until the end of 2017.
from.dat <- as.Date("12/01/12", format="%m/%d/%y") 
to.dat <- as.Date("01/31/19", format="%m/%d/%y") 

#uses try/catch for all 1500 stocks. Will skip if it cannot download or does not have
#data points for the time period
for (i in 1:length(stock_list)){
  test <- tryCatch(get_stock(stock_list[i]),
                   error = function(e) {
                     stock_error <- c(stock_error, stock_list[i])
                     return(e)} )
  if(!inherits(test, "error")){
    print(paste(stock_list[i], "works"))
    varname <- paste0(stock_list[i], ".pct_change")
    #calculating percent change using (today_adjusted/last_month_adjusted) - 1
    test <- mutate(test, !!varname := test[, paste0(str_replace(stock_list[i], "-", "."), ".Adjusted")]
                   /dplyr::lag(test[, paste0(str_replace(stock_list[i], "-", "."), ".Adjusted")], 1) - 1)
    # number of months/rows should be 73
    if (nrow(test) == 74){
      df <- cbind(df, i = test[, paste0(stock_list[i], ".pct_change")])
      names(df)[ncol(df)] <- stock_list[i]
    }
    else{
      print(paste(stock_list[i],"not in time period"))
      stock_error <- c(stock_error, stock_list[i])
    }
    
  }
}


### getting fama french data directly from their website
base <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
factor <- "Global_3_Factors"
format<- "_CSV.zip"

full_url <- glue(base, factor, format, sep="")

temp <- tempfile()
download.file(full_url, temp, quiet=TRUE)

Global_3_Factors <- 
  read_csv(unz(temp, "Global_3_Factors.csv"), 
           skip = 6) %>%
  rename(date = X1) %>% 
  mutate_at(vars(-date), as.numeric) %>% 
  mutate(date = 
           ymd(parse_date_time(date, "%Y%m"))) 

Global_3_Factors <- data.frame(Global_3_Factors)

ff <- Global_3_Factors[Global_3_Factors$date >= from.dat
                       & Global_3_Factors$date <= to.dat,]

ff <- na.omit(ff)
### finish getting fama french data


row.names(df) <- ff$date

#fama french data is in percent, so we divide by 100 to get decimal value
df <- cbind(df, ff$Mkt.RF / 100)
names(df)[ncol(df)] <- "MKT - RF"
df <- cbind(df, ff$RF/100)
names(df)[ncol(df)] <- "RF"
df <- cbind(df, ff$SMB / 100)
names(df)[ncol(df)] <- "SMB"
df <- cbind(df, ff$HML / 100)
names(df)[ncol(df)] <- "HML"

# drop first row (december month)
df <- df[-1,]
df <- df[ , colSums(is.na(df)) == 0]
write.csv(df, file = "returns.csv")

rownames(df) <- as.Date(rownames(df))

#for analysis, we are only observing until December 31, 2016 to calculate betas
df_2017_1 <- df[rownames(df) >= from.dat & rownames(df) <= as.Date("12/31/17", format="%m/%d/%y"),]

beta_df <- data.frame(stringsAsFactors = FALSE)

x1 <- df_2017_1$`MKT - RF`
x2 <- df_2017_1$SMB
x3 <- df_2017_1$HML

#performing linear regression to find the individual betas
for (i in 1:(length(df_2017_1) -4)){
  y <- df_2017_1[, colnames(df_2017_1)[i]] - df_2017_1$RF
  reg <- lm(y~x1+x2+x3)
  c <- data.frame(str_replace(colnames(df_2017_1)[i], "_return", ""), reg$coefficients[1], reg$coefficients[2], reg$coefficients[3], reg$coefficients[4])
  beta_df <- rbind(beta_df, c)
}
rownames(beta_df) <- seq(length=nrow(beta_df))
colnames(beta_df) <- c("stock", "alpha", "b1", "b2", "b3")

b_1 <- arrange(beta_df, desc(b1))
head(b_1, n=50)
#delete first rows that have erroneous data

#manually checked the first 50 stocks for the time period to check for erroneous data
b_1 <- b_1[-c(1,2,3,4,5,6,7,9),]

write.csv(b_1, file="highest_betas.csv")


####################################################################################
###part 2: machine learning#########################################################

#install.packages(c("quantmod", "dplyr", "readr", "data.table", "e1071", "xts", "fportfolio"))

#predicts with a lag of 2 months. 

highest_betas <- read_csv("highest_betas.csv")
stock_list <- highest_betas$stock

#we are collecting all data for these dates, so we can get actual return_lead data
#however, we do not use lead data in 2019 for the machine learning
from.dat <- as.Date("07/01/11", format="%m/%d/%y") 
to.dat <- as.Date("03/01/19", format="%m/%d/%y") 

get_stock2 <- function(stock){
  stock_data <- getSymbols(stock, src="yahoo", 
                           from = from.dat, to = to.dat, periodicity="monthly", auto.assign = FALSE) %>% data.frame() 
  colnames(stock_data) <- str_replace(colnames(stock_data), paste0(as.character(stock),"."), "")
  stock_data$stock <- as.character(stock)
  setDT(stock_data, keep.rownames = TRUE)[]
  setnames(stock_data, 1, "date")
  stock_data$date <- as.Date(stock_data$date, "%Y-%m-%d")
  
  #feature extraction
  stock_data$return <- with(stock_data, stock_data$Adjusted/dplyr::lag(stock_data$Adjusted, 1) -1)
  stock_data$return_lag1 <- dplyr::lag(stock_data$return, 1)
  stock_data$return_lag2 <- dplyr::lag(stock_data$return, 2)
  stock_data$return_lag3 <- dplyr::lag(stock_data$return, 3)
  stock_data$return_lag4 <- dplyr::lag(stock_data$return, 4)
  stock_data$return_lag5 <- dplyr::lag(stock_data$return, 5)
  
  stock_data$open_change <- with(stock_data, stock_data$Open/dplyr::lag(stock_data$Open, 1) -1)
  stock_data$high_change <- with(stock_data, stock_data$High/dplyr::lag(stock_data$High, 1) -1)
  stock_data$close_change <- with(stock_data, stock_data$Close/dplyr::lag(stock_data$Close, 1) -1)
  stock_data$low_change <- with(stock_data, stock_data$Low/dplyr::lag(stock_data$Low, 1) -1)
  
  web <- read_html(paste0("https://finance.yahoo.com/quote/",stock,"/profile"))
  web_get <- html_nodes(web, '[data-reactid="23"]') %>% html_text()
  stock_data$sector <- web_get[3]
  web_get <- html_nodes(web, '[data-reactid="27"]') %>% html_text()
  stock_data$industry <- web_get[3]
  
  # our 'y' is next month's return, will be 1 if positive, -1 if negative
  stock_data$return_lead <- with(stock_data, stock_data$Adjusted/dplyr::lag(stock_data$Adjusted, 1) -1) %>% dplyr::lead(2)
  stock_data$y <- as.factor(ifelse(stock_data$return_lead > 0, 1, -1))
  stock_data <- stock_data %>% select(date, stock, sector, industry, Open, High, Low, Close, Volume, Adjusted,
                                      open_change, high_change, close_change, low_change,
                                      return, return_lag1, return_lag2, return_lag3, return_lag4, return_lag5, y)
  return(stock_data)
}

get_stock3 <- function(stock, start_date, end_date){
  #returns the actual return for the next month (period of one month starting from the start date)
  stock_data <- getSymbols(stock, src="yahoo", 
                           from = start_date, to = end_date, periodicity="monthly", auto.assign = FALSE) %>% data.frame() 
  colnames(stock_data) <- str_replace(colnames(stock_data), paste0(as.character(stock),"."), "")
  stock_data$stock <- as.character(stock)
  setDT(stock_data, keep.rownames = TRUE)[]
  setnames(stock_data, 1, "date")
  stock_data$date <- as.Date(stock_data$date, "%Y-%m-%d")
  
  stock_data$return <- with(stock_data, stock_data$Adjusted/dplyr::lag(stock_data$Adjusted, 1) -1)
  stock_data$return_lead <- with(stock_data, stock_data$Adjusted/dplyr::lag(stock_data$Adjusted, 1) -1) %>% dplyr::lead(2)
  
  stock_data <- stock_data %>% select(date, stock, Close, return, return_lead)
  return(stock_data$return_lead[stock_data$date == start_date])
  #return(stock_data)
}

df2 <- data.frame()

for (i in 1:30){
  temp <- get_stock2(stock_list[i])
  df2 <- bind_rows(df2, temp)
}
df2$stock <- as.factor(df2$stock)
df2$sector <- as.factor(df2$sector)
df2$industry <- as.factor(df2$industry)


#df2 <- na.omit(df2)
str(df2)

#training data is before 2017, test data is 2018
train <- (df2$date < "2018-01-01")
df2.train <- df2[train,]
df2.test <- df2[!train,]

df2.train <- na.omit(df2.train)

set.seed(1)
#using crossvalidation for svm
tune.out <- tune(svm ,y~.,data=df2.train, kernel ="linear",
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))
bestmod <- tune.out$best.model
summary(bestmod)
ypred <- predict(bestmod, df2.test)
#table(predict=ypred, truth=df2.test$y)

#tune.out2 <- tune(svm, y~.,data=df2.train, kernel ="radial",
#                  ranges=list(cost=c(0.1,1,10,100,1000),
#                              gamma=c(0.5,1,2,3,4)))
#bestmod2 <- tune.out2$best.model
#ypred2 <- predict(bestmod2, df2.test)
#table(predict=ypred2, truth=df2.test$y)
#compared linear and radial svm, radial svm performed better

#had to add extra dates at the end of the vector when comparing the actual return leading data
dates_2018 <- c("2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01","2018-06-01", 
"2018-07-01", "2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01", "2019-02-01", "2019-03-01")

for (i in 1:13){
  cat("data from",dates_2018[i],"prediction for",dates_2018[i+2],"\n")
  
  df2_temp <- df2[df2$date == dates_2018[i],]
  if (i == 13)
    df2_temp$y <- 0
  df2_temp$ypred <- predict(bestmod, df2_temp)
  if (i != 13){
    print(table(predict=df2_temp$ypred, truth=df2_temp$y))
  }
  ticker <- df2_temp$stock[which(df2_temp$ypred == 1)] %>% as.vector()
  print(ticker)
  
  df_a <- df_2017_1 %>% select(ticker)
  rownames(df_a) <- as.Date(rownames(df_a))
  df_a <- as.timeSeries(df_a)
  
  effFrontier <- portfolioFrontier(df_a, constraints = "LongOnly")
  
  plot(effFrontier,c(1,2,3,4))
  
  #Plot Frontier Weights (Can Adjust Number of Points)
  frontierWeights <- getWeights(effFrontier) # get allocations for each instrument for each point on the efficient frontier
  colnames(frontierWeights) <- ticker
  risk_return <- frontierPoints(effFrontier)
  
  riskReturnPoints <- frontierPoints(effFrontier) # get risk and return values for points on the efficient frontier
  
  plot(riskReturnPoints)
  
  # plot Sharpe ratios for each point on the efficient frontier
  riskFreeRate <- 0.002
  plot((riskReturnPoints[,"targetReturn"]-riskFreeRate) / riskReturnPoints[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")
  
  #Plot Frontier Weights (Need to transpose matrix first)
  barplot(t(frontierWeights), main="Frontier Weights", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(frontierWeights))
  
  tangencyPort <- tangencyPortfolio(df_a, spec=portfolioSpec(), constraints="LongOnly")
  #tangencyPort
  
  tangencyweights <- getWeights(tangencyPort)
  print(tangencyweights)
  
  future_returns <- double()
  
  for (j in ticker){
    future_returns <- c(future_returns, get_stock3(j, dates_2018[i], dates_2018[i+2]))
  }
  
  dft <- data.frame(tangencyweights)
  assets <- colnames(frontierWeights)
  ggplot(data=dft, aes(x=assets, y=tangencyweights, fill=assets)) +
    geom_bar(stat="identity", position=position_dodge(),colour="black") +
    geom_text(aes(label=sprintf("%.02f %%",tangencyweights*100)),
              position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
    ggtitle("Tangency Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
    labs(x= "Assets", y = "Weight (%)")
  dft$actual_returns <- future_returns
  write.csv(dft, file=paste0("weights_",i,".csv"))
}