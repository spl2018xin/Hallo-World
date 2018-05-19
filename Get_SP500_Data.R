setwd("~/Desktop/fama french/FAMA official Data")
library(BatchGetSymbols)

# Get tickers for the SP500 stocks
df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$tickers

# Batch download data from Yahoo Finance
GA<- BatchGetSymbols(tickers = tickers,
                     first.date = "1990-01-01",
                     last.date = "2000-12-31")

# store summary info of the downloaded data (in the df.control sub-data.frame)
temp <- data.frame(GA$df.control$ticker, GA$df.control$total.obs, GA$df.control$threshold.decision)
head(temp)

# Test merging 2 data.frames by matching dates
# X <- data.frame(date = GA$df.tickers$ref.date[GA$df.tickers$ticker=="ABT"], GA$df.tickers$price.close[GA$df.tickers$ticker=="ABT"])
# Y <- data.frame(date = GA$df.tickers$ref.date[GA$df.tickers$ticker=="ACN"], GA$df.tickers$price.close[GA$df.tickers$ticker=="ACN"])
# result <- merge.data.frame(X, Y, by = "date", all.x = TRUE)

# ======================= USEFUL ===============================
# Find out the good tickers / #417 for now / #465 for 2010 - 2017 / #294 for 1990-2000
good.tickers <- GA$df.control$ticker[GA$df.control$threshold.decision=="KEEP"]

# Initialize SP500.data to have columns = number of good data series + 1 不需要初始化
# (the first column is for dates, stock data starts from the 2nd column / take max num of dates as total number of rows)
# SP500.data <- data.frame(
#              ncol = sum(GA$df.control$threshold.decision=="KEEP") + 1, 
#              nrow = max(GA$df.control$total.obs))

# Fill dates as the first stock "MMM" as it happens to have complete dates (column name = "date")
SP500.data<-data.frame(date = GA$df.tickers$ref.date[1:max(GA$df.control$total.obs)]) #潜在有 BUG

# colnames(SP500.data)[1]<-"Date"
# loop over the good data: read one ticker at a time and merge into SP500.data
# 之所以要 Merge 而不直接赋值数组元素是为了使用 Merge 函数匹配日期的功能
for(i in 1:length(good.tickers))
{
  # X is a temp dataframe that has 2 columns, 1st is date (for matching), 2nd is the actual data, e.g. closing price
  # Choose relevant data by matching tickers
  X <- data.frame(date = 
                    GA$df.tickers$ref.date[GA$df.tickers$ticker==good.tickers[i]], 
                    GA$df.tickers$price.adjusted[GA$df.tickers$ticker==good.tickers[i]])
  
  # change the column name of X to be the ticker of the stock
  # colnames(X)[2] = good.tickers[i] # this one don't work
  colnames(X)[2] <- GA$df.tickers$ticker[GA$df.tickers$ticker==good.tickers[i]]
  
  # merge X as a new column into SP500.data by matching date
  # missing dates will have NA by default
  SP500.data <- merge.data.frame(SP500.data, X, by = "date", all.x = TRUE)
}

write.csv(SP500.data, "SP500_price.adjusted_1990-2000.csv")

# Convert downloaded daily data to monthly
# Require quantmond library for the monthlyReturn() function
temp <- xts(SP500.data$AMZN, order.by = as.POSIXct(SP500.data$date))
temp.monthly <- monthlyReturn(temp)

# display the monthly data
temp.monthly 

# ======================= Automatic Download ===============================
# loop over above codes to download data from 1980 - 2015, group every 5 yrs.

library(lubridate)
List.of.start.date <- seq(as.Date("1980/1/1"), as.Date("2016/1/1"), "years")
List.of.start.date <- List.of.start.date[year(List.of.start.date)%%5==0]

Download.Stat <- data.frame(Data = List.of.start.date)
temp <- vector()

for(i in 1:(length(List.of.start.date)-1))
{
  start.date <- as.Date(List.of.start.date[i])
  end.date <- as.Date(List.of.start.date[i+1])-1
  
  # Download
  GA<- BatchGetSymbols(tickers = tickers,
                       first.date = start.date,
                       last.date = end.date)
  
  # Fill Date
  SP500.data<-data.frame(date = GA$df.tickers$ref.date[1:max(GA$df.control$total.obs)])
  good.tickers <- GA$df.control$ticker[GA$df.control$threshold.decision=="KEEP"]
  temp <- cbind(temp, length(good.tickers))
  
  for(i in 1:length(good.tickers))
  {
    X <- data.frame(date = 
                      GA$df.tickers$ref.date[GA$df.tickers$ticker==good.tickers[i]], 
                      GA$df.tickers$price.adjusted[GA$df.tickers$ticker==good.tickers[i]])
    
    # change the column name of X to be the ticker of the stock
    colnames(X)[2] <- GA$df.tickers$ticker[GA$df.tickers$ticker==good.tickers[i]]
    
    # merge X as a new column into SP500.data by matching date
    # missing dates will have NA by default
    SP500.data <- merge.data.frame(SP500.data, X, by = "date", all.x = TRUE)
  }
  
  file.name <- paste("SP500_price.adjusted_", paste(year(start.date), year(end.date), sep="-"), ".csv", sep="")
  write.csv(SP500.data, file.name)
}

Download.Stat <- Download.Stat[1:7,]
Download.Stat <- cbind(Download.Stat, Num.Tickers = data.frame(t(temp)))
