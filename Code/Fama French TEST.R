
summary(ffdata)
is.data.frame(ffdata)

head(ffdata)
# extract each column for each of the individual factors and for the fund return
rmrf<-ffdata[,2]
smb<-ffdata[,3]
hml<-ffdata[,4]
rf<-ffdata[,5]
ri<-ffdata[,6]

# excess return of the target fund
rirf<-ri-rf
# run a FF3 regression 
y<-lm(rirf~rmrf+smb+hml);y
summary(y)

# if we want to know to annulized 12

12*-.292 # -3.504 percent per year that the target fund underperformed over under over this five years
# period after adjusting for the risk of these three factors

1.21064 # Beta_rmrf/ the average stock has a beta of 1.0, the target fund is tilted towards having more
# market risk than the average stock or the avarage fund.

0.151 # (smb)small/ a small tilt towards small stocks but not a real large tilt there

-0.298# (hml: a value factor) negative/ the target fund is tilted towards growth stocks
# there are over weighting growth stocks relative to the market as a whole.

0.9509 #Rˆ2









#######################GLS, AR(1,1) and Maximum Likelyhood. 
#including Fraziini BAB and QMJ return factors together with Fama and French 5 factors. The parameters can be added or removed according to preference and type of investment strategy used.
# Extract Fama-French Factors + Momentum + Franziini Factors
rmrf <- ffdata[,2]/100
rf <- ffdata[,7]/100
smb <- ffdata[,3]/100
hml <- ffdata[,4]/100

rmw <- ffdata[,5]/100
cma <- ffdata[,6]/100

umd <- ffdata[,8]
bab <- ffdata[,9]/100
qmj <- ffdata[,10]/100

for(i in 11:18){ #Loop across rows of assets
  fund <- ffdata[,i]/100
  fund.xcess <- fund - rf
  ffgls <- gls(rirf ~ rmrf + smb + hml + rmw + cma,
               correlation=corARMA(p=1, q=1), method='ML')
  print(summary(ffgls))
}


######### learning how to download data 
# to download end of day trade data for several stocks in the financial market
install.packages("quantmod") #
library(quantmod)

# download data from Yahoo Finance
quantmod::getSymbols.yahoo("AAPL",env =.GlobalEnv,return.class = 'xts',
                           index.class  = 'Date', 
                           from = "2016-01-01", to = Sys.Date(),
                           periodicity = "weekly",curl.options = list())


install.packages('BatchGetSymbols')
library(BatchGetSymbols)
library(rvest)
library(xml2)
# set dates
first.date <- Sys.Date() - 60
last.date <- Sys.Date()

# set tickers
tickers <- c('FB','NYSE:MMM','PETR4.SA','abcdef')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()
print(l.out$df.control)

library(ggplot2)

p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)

###################
# Downloading data for all tickers in the SP500 index

library(BatchGetSymbols)

first.date <- Sys.Date()-365
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$tickers

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

print(l.out$df.control)
print(l.out$df.tickers)

### test: download data of apple and google
library(BatchGetSymbols)

tickers<-c('GOOG','AAPL')

first.date <- Sys.Date()-30
last.date <- Sys.Date()

GA<- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

print(GA)

AAPL<-data.frame(GA[["df.tickers"]][["price.adjusted"]],
                 GA[["df.tickers"]][["ticker"]])
AAPL
AAPL<-(AAPL[,4])
AAPL

############ test AAPL with FF3 有问题

# AAPL data
setwd("~/Desktop/fama french/FF")
AAPL <- read.csv("~/Desktop/fama french/FF/AAPL.CSV")
View(AAPL)

# have a brief view of the data

str(AAPL)
head(AAPL)
class(AAPL)  # dataframe

# sepcify the regression factors

RIRF<-AAPL[,7]
RMRF<-AAPL[,2]
SMB<-AAPL[,3]
HML<-AAPL[,4]

# run a ff3 regression
ff3<-lm(RIRF~RMRF+SMB+HML);ff3
summary(ff3)

######BRK_A
head(BRKA)
RIRF<-BRKA[,6]
RMRF<-BRKA[,2]
SMB<-BRKA[,3]
HML<-BRKA[,4]

ff_BRKA<-lm(RIRF~RMRF+SMB+HML);ff_BRKA
summary(ff_BRKA)

# correlation check
CorMat<-cor(BRKA)
View(CorMat)
corrplot(CorMat,method = "color")
corrplot(CorMat,method = "circle")



######BRK_A_2 daliy

head(BRK.A.2)
return<-log(BRK.A.2[,6])  
RIRF<-BRK.A.2[,14]  
RMRF<-BRK.A.2[,10]
SMB<-BRK.A.2[,11]
HML<-BRK.A.2[,12]

ff_BRKA2<-lm(RIRF~RMRF+SMB+HML);ff_BRKA2
summary(ff_BRKA2)

# correlation check
CorMat<-cor(BRK.A.2)
View(CorMat)
corrplot(CorMat,method = "color")
corrplot(CorMat,method = "circle")

####DAX有问题
head(BMW.DE)
attach(BMW.DE)
capm<-lm(RIRF~RMRF)
detach(BMW.DE)
summary(capm)