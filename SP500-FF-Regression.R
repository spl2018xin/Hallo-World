library(quantmod)

# Read SP500 daily data and convert date column to date format
SP500.data <- read.csv("SP500_price.adjusted_1990-2000.csv")
SP500.data$date <- as.Date(SP500.data$date)

# Read in SP500 company ticker information
Mapping <- read.csv("constituents.csv")
colnames(Mapping)[1] <- "Ticker"

# Current FF3 till 201803, monthly
FF3 <- read.csv("original/FF3.csv")

# Select 2010 - 2017 range to match FF3
#Stock.Prices.Daily <- SP500.data[SP500.data$date>="2010-01-01" & 
#                                   SP500.data$date<="2017-12-31",-1]

Stock.Prices.Daily <- SP500.data[SP500.data$date>="1990-01-01" & 
                                   SP500.data$date<="2000-12-31",-1]

# Convert series to XTS for using quantmod's monthlyReturn function
Stock.Prices.Daily <- xts(Stock.Prices.Daily[,-1], 
                          order.by = as.POSIXct(Stock.Prices.Daily$date))

# Remove stocks with NAs in the series, otherwise monthly Return will not work properly
Stock.Prices.Daily <- Stock.Prices.Daily[,colSums(is.na(Stock.Prices.Daily)) == 0]

# Apply monthlyReturn function to each column (it seems it converts only one column at a time)
Stock.Prices.Monthly <- do.call(cbind, lapply(Stock.Prices.Daily, monthlyReturn))
# Stock.Prices.Monthly <- na.omit(Stock.Prices.Monthly)
colnames(Stock.Prices.Monthly) <- colnames(Stock.Prices.Daily)

# Number of stocks left
ncol(Stock.Prices.Monthly)

# Stock.Prices.Monthly <- monthlyReturn(Stock.Prices.Daily) # This doesn't work
# temp <- xts(SP500.data$AMZN, order.by = as.POSIXct(SP500.data$date))
# temp.monthly <- monthlyReturn(temp)

# Select the monthly data of FF3 between 2010 - 2017
# FF <- FF3[FF3$X >= 201001 & FF3$X <= 201712,]
FF <- FF3[FF3$X >= 199001 & FF3$X <= 200012,]

# Regression!
# Instead of simply removing NAs, we shall try na.action = na.omit / na.exclude in lm function
# But still need a way to work around NAs when converting to monthly
# Another idea is to regress differently and run regression ticker by ticker
Results <- list()
for(i in 1:ncol(Stock.Prices.Monthly))
{
  RiRF <- Stock.Prices.Monthly[,i] - FF$RF
  Regression <- lm(RiRF ~ FF$Mkt.RF + FF$SMB + FF$HML)
  Results[[i]] <- summary(Regression)
}

# Results!
betas <- vector()
std.errors <- vector()
t.values <- vector()
p.values <- vector()
r.squareds <- vector()
adj.r.squareds <- vector()

for(i in 1:ncol(Stock.Prices.Monthly))
{
  betas <- cbind(betas,Results[[i]]$coefficients[,1])
  std.errors <- cbind(std.errors,Results[[i]]$sigma)
  t.values <- cbind(t.values, Results[[i]]$coefficients[,3])
  p.values <- cbind(p.values, Results[[i]]$coefficients[,4])
  
  r.squareds <- cbind(r.squareds, Results[[i]]$r.squared)
  adj.r.squareds <- cbind(adj.r.squareds, Results[[i]]$adj.r.squared)
  
}

Regression.results <- cbind(data.frame(colnames(Stock.Prices.Monthly)), 
                    t(r.squareds), t(adj.r.squareds), 
                    t(betas), t(p.values))

colnames(Regression.results) = c("Ticker", "R.Squared", "Adj.R.Squared", 
                         "Intercept", "Mkt.Rf", "SMB", "HML", 
                         "P(Intercept)", "P(Mkt.Rf)", "P(SMB)", "P(HML)")

# add name / sector to the results
Regression.results <- merge(x = Regression.results, y = Mapping, by = "Ticker", all.x = TRUE)

# select stocks with R2>=0.08
R2 <- Regression.results[Regression.results$R.Squared>=0.08,
                   c("Ticker","Name","Sector","R.Squared")]

# display the stocks with R2 larger than 8%
#R2[order(R2[,2], decreasing = T),]

R2[order(R2$R.Squared, decreasing = T),]
# count how many of them have R2 larger than 8%

# proportion of stocks with 8% R2
dim(R2)[1]/dim(Regression.results)[1]
boxplot(R2)


# boxplot of regression results
library(ggplot2)
library(reshape2)
num.stocks <- dim(Regression.results)[1]
plot.data <- Regression.results[ ,c("Intercept", "Mkt.Rf", "SMB", "HML")]
plot.melt <- melt(plot.data)

plot.data <- data.frame(Betas = rep(c("Intercept", "Mkt-Rf", "SMB", "HML"),rep(num.stocks, 4)))
plot.data$Level <- as.vector(cbind(
                    Regression.results$Intercept, 
                    Regression.results$Mkt.Rf, 
                    Regression.results$SMB, 
                    Regression.results$HML))
plot.data$P.Value<- as.vector(cbind(
                    Regression.results$`P(Intercept)`, 
                    Regression.results$`P(Mkt.Rf)`,
                    Regression.results$`P(SMB)`, 
                    Regression.results$`P(HML)`))

plot.melt <- melt(plot.data, "Betas")
ggplot(plot.melt, aes(x=Betas, y=value)) + geom_boxplot() + facet_wrap(~ variable, scales='free')

plot.2010 <- plot.melt

# meaningful?
plot(Stock.Prices.Monthly)
plot(Stock.Prices.Daily)

#test: plot monthly retrun of MMM
hist(Stock.Prices.Monthly$MMM,breaks = 30, freq=F,main="Histogramm, density curve (gaussian kernel) of MMM ")
lines(density(Stock.Prices.Monthly$MMM), col="red", lwd=2) 
curve(dnorm(x, mean = mean(Stock.Prices.Monthly$MMM), sd = sd(Stock.Prices.Monthly$MMM)), add=TRUE, col="blue",lwd=2)



#test: log retrun: MMM
# daily
Dlg_MMM <- diff(log(Stock.Prices.Daily$MMM), lag=1) 
plot(Dlg_MMM)

# monthly
Mlg_MMM <- diff(log(Stock.Prices.Daily$MMM), lag=30) #?
head(Mlg_MMM)
plot(Mlg_MMM)

M<-diff(log(Stock.Prices.Monthly$MMM), lag=1) 
plot(M)


# ======================= Automatic Regression ===============================
# loop over above codes to regress data from 1980 - 2015, group every 5 yrs.
library(lubridate)
List.of.start.date <- seq(as.Date("1980/1/1"), as.Date("2016/1/1"), "years")
List.of.start.date <- List.of.start.date[year(List.of.start.date)%%5==0]

# FF3: 192607 - 201803, monthly
FF3 <- read.csv("original/FF3.csv")

for(i in 1:(length(List.of.start.date)-1))
{
  start.date <- as.Date(List.of.start.date[i])
  end.date <- as.Date(List.of.start.date[i+1])-1
  
  # read data
  file.name <- paste("SP500_price.adjusted_", paste(year(start.date), year(end.date), sep="-"), ".csv", sep="")
  SP500.data <- read.csv(file.name)
  SP500.data$date <- as.Date(SP500.data$date)
  
  # remove first column "X" created due to importing  
  Stock.Prices.Daily <- SP500.data[SP500.data$date>= start.date & 
                                     SP500.data$date<= end.date,-1]
  
  FF <- FF3[FF3$X >= format.Date(start.date, "%Y%m") & FF3$X <= format.Date(end.date, "%Y%m"), ]
  
  # try a diff approach: loop over stocks and convert to monthly for each stock
  
}