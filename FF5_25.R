library(reshape)    # required for melt function in plotting
library(ggplot2)

# 25 Portfolios Formed on Size and Book-to-Market (5 x 5)
# Same as the FF3_25 one
# FF5 paper P13 Table 7

Stocks <- read.csv("Data/original/25_Portfolios_5x5_Value.CSV")
# Stocks <- read.csv("Data/original/25_Portfolios_5x5_Wout_Div.CSV")
FF5 <- read.csv("Data/original/F-F_Research_Data_5_Factors_2x3.csv")

start.date <- "196307"
end.date <- "201312"

Stocks <- Stocks[paste(Stocks$X) >= start.date & paste(Stocks$X) <= end.date, ]
FF5 <- FF5[FF5$X >= start.date & FF5$X <= end.date, ]

betas <- vector()
t.values <- vector()
for(i in 2:ncol(Stocks))
{
  RiRF <- Stocks[,i]-FF5$RF
  y <- lm(RiRF ~ FF5$Mkt.RF + FF5$SMB + FF5$HML + FF5$RMW + FF5$CMA)
  betas <- cbind(betas, summary(y)$coefficients[,1])
  t.values <- cbind(t.values, summary(y)$coefficients[,3])
}

colnames(betas) <- colnames(Stocks[-1])
colnames(t.values) <- colnames(Stocks[-1])

resize <- function(x)
{
  df = data.frame(matrix(x, nrow=5, byrow = TRUE))
  colnames(df) = c("Low", "2", "3", "4", "High")
  rownames(df) = c("Small", "2", "3", "4", "Big")
  return(df)
}

for(i in 1:6)
{
  print(rownames(betas)[i])
  cat("beta")
  print(resize(betas[i,]))
  cat("t-value")
  print(resize(t.values[i,]))
  print("------------------")
}


