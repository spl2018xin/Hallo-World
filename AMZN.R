library(quantmod)

getSymbols("AMZN", from = as.Date("2010-09-30"), to = as.Date("2017-10-01"))
# getSymbols("NKE", from = as.Date("2000-10-01"), to = as.Date("2017-10-01"))
# getSymbols("F", from = as.Date("2000-10-01"), to = as.Date("2017-10-01"))


# Ri has monthly return of AMZN from 2010.10 to 2017.10 (including both Octobers)
Ri <- data.frame(monthlyReturn(AMZN))
# Ri <- data.frame(monthlyReturn(NKE))
# Ri <- data.frame(monthlyReturn(F))

# Select corresponding Fama French Factors
FF <- FF3[FF3$X>=201010 & FF3$X<=201710,]

# Calculate RiRf
RiRf <- unlist(Ri - FF$RF)

# Regression
y <- lm(RiRf ~ FF$Mkt.RF + FF$SMB + FF$HML)
summary(y)

# Convert downloaded daily data to monthly
# Require quantmond library for the monthlyReturn() function
# Change data format to XTS to use monthlyReturn() function
temp <- xts(SP500.data$AMZN, order.by = as.POSIXct(SP500.data$date))
temp.monthly <- monthlyReturn(temp)

# FF3:SMB and HML for July of year t to June of t+1 include all NYSE, AMEX, and NASDAQ stocks 
# for which market equity data for December of t-1 and June of t, and (positive) book equity data for t-1.

# AMZN monthlyReturn 2010-Okt~2017-Okt # end monthly return correspond to the corresponding return.
# 2010-10-01~2017-10-01: 84 obs. start from 29.10 to 29.09: wrong!
# 2010-10-01~2017-10-30: 85 obs. start from 29.10 to 27.10: R-squared:  0.2658, ARsquare: 0.2387
# 2010-10-29~2017-10-30: 85 obs. start from 29.10 to 27.10: R-squared:  0.2536, ARsquare: 0.2259 # why different
