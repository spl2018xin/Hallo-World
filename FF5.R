library(xts)        # for using >= in date format
# library(lubridate)  # for using ymd to convert char to date
library(quantmod)
library(reshape)    # required for melt function in plotting
library(ggplot2)

start.date <- "2010-01-01"
end.date <- "2017-12-31"

# read FF5 data. 
# must pre-process in Excel: the downloaded data has 3 rows of headers and 
# also yearly data at the last few rows
FF5 <- read.csv("original/F-F_Research_Data_5_Factors_2x3.csv")

# Read SP500 daily data and convert date column to date format
# SP500.data <- read.csv("SP500_price.adjusted_2000-2018.csv")
SP500.data <- read.csv("SP500_price.adjusted_2010-2017.csv")
SP500.data$date <- as.Date(SP500.data$date)

# Read in SP500 company ticker information
Mapping <- read.csv("constituents.csv")
colnames(Mapping)[1] <- "Ticker"

Stock.Prices.Daily <- SP500.data[SP500.data$date >= start.date & 
                                 SP500.data$date <= end.date, -1]

# Converting the price series to XTS format 
# Convert to monthly in the loop for EACH stock, because some stocks have NAs at the beginning
# => different stocks will have different price points available
Stock.Prices.Daily <- xts(Stock.Prices.Daily[,-1], 
                          order.by = as.POSIXct(Stock.Prices.Daily$date))

# Data contains number of stocks plus one column of dates
Results <- data.frame()
for(i in 1:ncol(Stock.Prices.Daily))
{
  # RiRF <- Stock.Prices.Daily[,i+1] - FF$RF
  # RMW: Robust Minus Weak
  # CMA: Conservative Minus Aggressive
  # na.omit: omit the NA entries, we only need to omit NA when applying in a batch for daily results
  # since monthlyReturn() function do not work with NAs
  # therefore, when converting price to monthly return, we need to manually pick out the non-NA series
  # before converting, hence no need to omit NAs in the regression itself.
  # Regression <- lm(RiRF ~ FF$Mkt.RF + FF$SMB + FF$HML + FF$RMW + FF$CMA, na.action=na.omit)
  
  # filter non-NA entries and convert price series to monthly returns.
  Ri <- Stock.Prices.Daily[, i]
  Ri <- Ri[!is.na(Ri),]
  Ri <- monthlyReturn(Ri)
  
  # matching FF data
  FF <- FF5[FF5$X >= format(index(head(Ri, n=1)), "%Y%m") & 
            FF5$X <= format(index(tail(Ri, n=1)), "%Y%m"), ]
  
  RiRF <- Ri - FF$RF
  
  # RMW: Robust Minus Weak
  # CMA: Conservative Minus Aggressive
  Regression <- lm(RiRF ~ FF$Mkt.RF + FF$SMB + FF$HML + FF$RMW + FF$CMA)
  S <- summary(Regression)
  
  # Only read-out betas, p-values and r.squared
  Results <- rbind(Results, cbind(colnames(Stock.Prices.Daily)[i],length(Ri),
                              data.frame(t(S$coefficients[,1])),
                              data.frame(t(S$coefficients[,4])),
                              data.frame(t(S$r.squared))))
  # remove local variable
  rm(Ri, RiRF, FF, Regression, S)
}

colnames(Results) <- c("Ticker", "Num.of.Months",
                       "Intercept", "Mkt-Rf", "SMB", "SML", "RMW", "CMA",
                       "P(Intercept)", "P(Mkt-Rf)", "P(SMB)", "P(SML)", "P(RMW)", "P(CMA)",
                       "R-squared")

# add sector to results
Results <- merge(x = Results, y = Mapping, by = "Ticker", all.x = TRUE)

# melt and plot
df.melt <- melt(Results[,c("R-squared", "Sector")], "Sector")
# df.melt <- melt(Results[,c("P(Intercept)", "P(RMW)", "P(CMA)", "Sector")], "Sector")
ggplot(df.melt, aes(x=Sector, y=value)) + geom_boxplot() + facet_wrap(~ variable, scales='free')

# check firms with the largest R-squared
head(Results[order(Results$`R-squared`, decreasing = T),], n=10)
# check results with Google
Results[Results$Ticker == "GOOGL",]
