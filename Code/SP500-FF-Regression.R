library(zoo)
library(quantmod)
library(xts)

# Read SP500 daily data and convert date column to date format
SP500.data      = read.csv("Downloaded Stock Prices/SP500_price.adjusted_2010-2017.csv")
SP500.data$date = as.Date(SP500.data$date)

# Read in SP500 company ticker information
Mapping              = read.csv("Downloaded Stock Prices/constituents.csv")
colnames(Mapping)[1] = "Ticker"

# Current FF3 till 201803, monthly
FF3 = read.csv("Downloaded Stock Prices/FF3.csv")

# Select 2010 - 2017 range to match FF3
Stock.Prices.Daily = SP500.data[SP500.data$date>="2010-01-01" & 
                                   SP500.data$date<="2017-12-31", -1]

#=====return
# Calculate period returns
Stock.Returns            = tail(Stock.Prices.Daily[ ,-1], n = 1) / head(Stock.Prices.Daily[ ,-1], n = 1) - 1
rownames(Stock.Returns)  = "Return.2010.2017"

# Transpose for easier analysis
Stock.Returns <- t(Stock.Returns)
Stock.Returns <- data.frame(Ticker = rownames(Stock.Returns), Period.Return = Stock.Returns)

Stock.Returns <- merge(Stock.Returns, Mapping, by = "Ticker", all.x = T)
# temp <- Stock.Returns[order(Stock.Returns$Return.2010.2017, decreasing = T),]

#=====end


# Convert series to XTS for using quantmod's monthlyReturn function
Stock.Prices.Daily <- xts(Stock.Prices.Daily[,-1], 
                          order.by = as.POSIXct(Stock.Prices.Daily$date))


# Remove stocks with NAs in the series, otherwise monthly Return will not work properly
Stock.Prices.Daily <- Stock.Prices.Daily[,colSums(is.na(Stock.Prices.Daily)) == 0]


# Apply monthlyReturn function to each column (it seems it converts only one column at a time)
Stock.Returns.Monthly <- do.call(cbind, lapply(Stock.Prices.Daily, monthlyReturn))

# Stock.Returns.Monthly <- na.omit(Stock.Returns.Monthly)
colnames(Stock.Returns.Monthly) <- colnames(Stock.Prices.Daily)



# Select the monthly data of FF3 between 2010 - 2017
FF <- FF3[FF3$X >= 201001 & FF3$X <= 201712,]




#============= Regression!

# Instead of simply removing NAs, we shall try na.action = na.omit / na.exclude in lm function
# But still need a way to work around NAs when converting to monthly
# Another idea is to regress differently and run regression ticker by ticker
Results <- list()
for(i in 1:ncol(Stock.Returns.Monthly))
{
  RiRF <- Stock.Returns.Monthly[,i] - FF$RF
  Regression <- lm(RiRF ~ FF$Mkt.RF + FF$SMB + FF$HML)
  Results[[i]] <- summary(Regression)
}

#===============

# Results!
betas <- vector()
std.errors <- vector()
t.values <- vector()
p.values <- vector()
r.squareds <- vector()
adj.r.squareds <- vector()

for(i in 1:ncol(Stock.Returns.Monthly)) {
  betas <- cbind(betas,Results[[i]]$coefficients[,1])
  std.errors <- cbind(std.errors,Results[[i]]$sigma)
  t.values <- cbind(t.values, Results[[i]]$coefficients[,3])
  p.values <- cbind(p.values, Results[[i]]$coefficients[,4])
  
  r.squareds <- cbind(r.squareds, Results[[i]]$r.squared)
  adj.r.squareds <- cbind(adj.r.squareds, Results[[i]]$adj.r.squared)
  
}

Regression.results <- cbind(data.frame(colnames(Stock.Returns.Monthly)), 
                    t(r.squareds), t(adj.r.squareds), 
                    t(betas), t(p.values))

colnames(Regression.results) = c("Ticker", "R.Squared", "Adj.R.Squared", 
                         "Intercept", "Mkt.Rf", "SMB", "HML", 
                         "P(Intercept)", "P(Mkt.Rf)", "P(SMB)", "P(HML)")

# add name / sector to the results
Regression.results <- merge(x = Regression.results, y = Mapping, by = "Ticker", all.x = TRUE)
Regression.results2 <- merge(x = Stock.Returns, y = Regression.results, by = "Ticker", all.x = TRUE)


# Display R-square 
R2 <- Regression.results[ , c("Ticker","Name","Sector","R.Squared")]

# display the stocks with R2 larger than 8%
#R2[order(R2[,2], decreasing = T),]

head(R2[order(R2$R.Squared, decreasing = T),], n=10)



# boxplot of regression results
library(ggplot2)
library(reshape2)
num.stocks <- dim(Regression.results)[1]
#plot.data <- Regression.results[ ,c("Intercept", "Mkt.Rf", "SMB", "HML")]
#plot.melt <- melt(plot.data)

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
# reordering the variables don't work yet
# plot.melt <- factor(plot.melt, levels = c("Intercept", "Mkt-Rf", "SMB", "HML"))
ggplot(plot.melt, aes(x=Betas, y=value)) + geom_boxplot() + facet_wrap(~ variable, scales='free')

plot.2010 <- plot.melt

#=================


# ====================== Additional Analysis =============================

# Period Stock.Returns are calculated above in line 20

Regression.results2 <- Regression.results2[!is.na(Regression.results2$R.Squared),]
Top20 <- head(Regression.results2[order(Regression.results2$Return.2010.2017, decreasing = T),], n=20)
Bottom20 <- head(Regression.results2[order(Regression.results2$Return.2010.2017),], n=20) # WELL is NA

View(Top20)
View(Bottom20)

#====correlation plot
# Select the numeric columns
# Top 20
M <- cor(Top20[,c(2,6:15)])
# Bottom 20
M <- cor(Bottom20[,c(2,6:15)])
M <- cor(Regression.results2[,c(2,6:15)])

library(corrplot)
corrplot(M, type = "upper")
# corrplot(M, method = "number", type = "upper")

# ======================= Batch Regression ===============================
# loop over above codes to regress data from 1980 - 2015, group every 5 yrs.
library(lubridate)
List.of.start.date <- seq(as.Date("1980/1/1"), as.Date("2016/1/1"), "years")
List.of.start.date <- List.of.start.date[year(List.of.start.date)%%5 == 0]

# FF3: 192607 - 201803, monthly
FF3 <- read.csv("Downloaded Stock Prices/FF3.CSV")

# Take the common tickers across all periods
# Fix.Tickers <- Reduce(intersect, list(Descriptions[[1]]$Ticker, 
# Descriptions[[2]]$Ticker, Descriptions[[3]]$Ticker, Descriptions[[4]]$Ticker, 
# Descriptions[[5]]$Ticker, Descriptions[[6]]$Ticker, Descriptions[[7]]$Ticker))

# Each batch stores results for a 5yr group
Batch <- list()
Descriptions <- list()

Beta.batch <- list()
max.smb <- 0
ticker.smb <- NULL

for(i in 1:(length(List.of.start.date)-1))
{
  start.date <- as.Date(List.of.start.date[i])
  end.date <- as.Date(List.of.start.date[i+1])-1
  print(paste(start.date, end.date,sep=" - "))
  
  # read data
  file.name <- paste("Downloaded Stock Prices/SP500_price.adjusted_", paste(year(start.date), year(end.date), sep="-"), ".csv", sep="")
  SP500.data <- read.csv(file.name)
  SP500.data$date <- as.Date(SP500.data$date)
  
  # remove first column "X" created due to importing  
  Stock.Prices.Daily <- SP500.data[SP500.data$date>= start.date & 
                                     SP500.data$date<= end.date,-1]
  
  # Convert series to XTS for using quantmod's monthlyReturn function
  Stock.Prices.Daily <- xts(Stock.Prices.Daily[,-1], 
                            order.by = as.POSIXct(Stock.Prices.Daily$date))
  
  # select only tickers surviving all periods
  # Stock.Prices.Daily <- Stock.Prices.Daily[,Fix.Tickers]
  
  # try a diff approach: loop over stocks and convert to monthly for each stock
  
  # initialize
  Results <- list()
  Description <- data.frame()
  
  betas <- data.frame()
  
  # loop through stocks
  for(j in 1:ncol(Stock.Prices.Daily))
  {
    # The j-th stock
    Rj <- Stock.Prices.Daily[,j]
    
    cat(colnames(Stock.Prices.Daily[,j]), " ")
    # non-NA entries
    Rj <- Rj[!is.na(Rj),]
    Rj <- monthlyReturn(Rj)
    
    # matching FF data
    FF <- FF3[FF3$X >= format(index(head(Rj, n=1)), "%Y%m") & 
              FF3$X <= format(index(tail(Rj, n=1)), "%Y%m"), ]
    
    # Rj is now RjRF
    Rj <- Rj-FF$RF
    Regression <- lm(Rj ~ FF$Mkt.RF + FF$SMB + FF$HML)
    Results[[j]] <- summary(Regression)
    Description <- rbind(Description, 
                         data.frame(colnames(Stock.Prices.Daily[,j]), 
                                    format(index(head(Rj, n=1)), "%Y%m"), 
                                    format(index(tail(Rj, n=1)), "%Y%m"), 
                                    length(Rj)))
    
    if(summary(Regression)$coefficients[3,1]>max.smb){
      max.smb <- summary(Regression)$coefficients[3,1]
      ticker.smb <- colnames(Stock.Prices.Daily)[j]
    }
      
    
    # Batch[[1]][[2]]$coefficients[,1]
    # try read-out results at regression time
    # betas, p-values, r-squareds
    betas <- rbind(betas, cbind(data.frame(t(Results[[j]]$coefficients[,1])),
                                data.frame(t(Results[[j]]$coefficients[,4])),
                                data.frame(t(Results[[j]]$r.squared))))
  }
  print("")
  
  # Save all regression summaries
  Batch[[i]] <- Results
  
  # Save the ticker / dates for ease of tracking the regression summary
  colnames(Description) = c("Ticker", "Start.Month", "End.Month", "Number.of.Months")
  Descriptions[[i]] <- Description
  
  # Save the regression results for plotting
  colnames(betas) <- c("Intercept", "Mkt-Rf", "SMB", "SML", 
                       "P(Intercept)", "P(Mkt-Rf)", "P(SMB)", "P(SML)",
                       "R-squared")
  
  # Try rbind here instead of list for convenience of melt.
  Beta.batch[[i]] <- betas
  
  # remove temp variables
  rm(Description)
  rm(Results)
  rm(Regression)
  rm(Rj)
  
  rm(betas)
}

#======== end of the regression


# Boxplot
# Combine all batches together into one large dataframe
df <- data.frame()
Num.Obs <- data.frame()

for(i in 1:(length(List.of.start.date)-1))
{
  start.date <- as.Date(List.of.start.date[i])
  end.date <- as.Date(List.of.start.date[i+1])-1
  # print(paste(year(start.date), year(end.date),sep="-"))
  label <- paste(year(start.date), year(end.date),sep="-")
  df <- rbind(df, cbind(rep(label, dim(Beta.batch[[i]])[1]), Beta.batch[[i]]))
  Num.Obs <- rbind(Num.Obs, cbind(  paste(year(start.date), year(end.date),sep="-"), dim(Beta.batch[[i]])[1]))
}

colnames(df) <- c("Year",
                  "Intercept", "Mkt-Rf", "SMB", "SML", 
                  "P(Intercept)", "P(Mkt-Rf)", "P(SMB)", "P(SML)",
                  "R-squared")

df.melt <- melt(df, "Year")
ggplot(df.melt, aes(x=Year, y=value)) + geom_boxplot() + facet_wrap(~ variable, scales='free')

colnames(Num.Obs) <- c("Time Period", "Number of Stocks")

