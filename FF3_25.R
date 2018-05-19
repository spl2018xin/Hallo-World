
#1# setting the working directory and importing the data

setwd("~/Desktop/fama french/FAMA official Data")
library(readxl)
# P25<- read_excel("25 Average Equal Weighted Returns -- Monthly.xlsx")
FF3<- read_excel("FF3_196307-199112.xlsx")
P25<- read_excel("FF3_25_ValueWeighted.xlsx")

#2# data overview

head(P25)
head(FF3)


#3# read regression factors 
# unlist: convert the data into vector format
rmrf<-unlist(FF3[,2])
smb<-unlist(FF3[,3])
hml<-unlist(FF3[,4])
rf<-unlist(FF3[,5])
ri<-unlist(P25[,2])
rirf<-unlist(ri-rf) # excess return of the target fund
rirf 


#4# run a regression for test purpose
y<-lm(rirf~rmrf+smb+hml);y 
summary(y)


  
# Start batch regressing 25 portfolios 
results <- list()
# The first column of P25 is year, not data
for(i in 1:(ncol(P25)-1))
{
  rirf<-unlist(P25[,i+1])-rf # Data starts from the 2nd col of P25 as 1st col is year.
  y<-lm(rirf~rmrf+smb+hml)
  results[[i]]<-summary(y)
}

betas <- vector()
std.errors <- vector()
t.values <- vector()
R.squareds <- vector()
# save all betas 
for(i in 1:(ncol(P25)-1))
{
  betas <- cbind(betas,results[[i]]$coefficients[,1])
  std.errors <- cbind(std.errors,results[[i]]$sigma)
  t.values <- cbind(t.values, results[[i]]$coefficients[,3])
  R.squareds <- cbind(R.squareds, results[[i]]$adj.r.squared)
}

# resize the output to 5x5 format like Fama French paper
resize <- function(x)
{
  return(matrix(x, nrow=5, byrow = TRUE))
}
# resize alpha
alpha <- resize(betas[1,])
alpha
# resize beta
market.beta <- resize(betas[2,])
SMB.beta <- resize(betas[3,])
HML.beta <- resize(betas[4,])

# display betas
market.beta
SMB.beta
HML.beta

# resize t-stats
market.t <-resize(t.values[2,])
SMB.t <- resize(t.values[3,])
HML.t <- resize(t.values[4,])

market.t

# resize R-squareds
resize(R.squareds)
resize(std.errors)
