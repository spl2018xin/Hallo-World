##################################################################
#                                                                #
#     Replication of the Fama French Three Factor Model          #
#                                                                #
##################################################################

#1# Date Preparation

library(readxl)
# FF3 is the monthly data collection of the three factors from 1963/07 to 1991/12
FF3 <- read_excel("Data/FF3_196307-199112.xlsx")

# P25 is value-weighted 5*5 portfolios from 1963/07 to 1991/12
P25 <- read_excel("Data/FF3_25_ValueWeighted.xlsx")


#2# read regression factors

# unlist: convert the data into vector format
rmrf <- unlist (FF3[, 2])
smb <- unlist (FF3[, 3])
hml <- unlist (FF3[, 4])
rf <- unlist (FF3[, 5])
ri <- unlist (P25[, 2])
rirf <- unlist (ri-rf) # excess return of the target fund
rirf 


#3# Start batch regressing 25 portfolios 
results <- list()

# Data starts from the 2nd col of P25
for (i in 1:(ncol (P25) - 1)) { 
  rirf <- unlist (P25[, i + 1]) - rf   
     y <- lm (rirf ~ rmrf + smb + hml)
  results[[i]] <- summary (y)
}


# set up empty vectors

betas <- vector ()
std.errors <- vector ()
t.values <- vector ()
R.squareds <- vector ()

# save all betas' regression results

for(i in 1: (ncol (P25) - 1)) {
       betas <- cbind (betas, results[[i]]$coefficients[, 1])
  std.errors <- cbind (std.errors,results[[i]]$sigma)
    t.values <- cbind (t.values, results[[i]]$coefficients[, 3])
  R.squareds <- cbind (R.squareds, results[[i]]$adj.r.squared)
}

# give names
colnames(betas) <- colnames(P25)[-1]


#4#   Resize the output to 5x5 format like Fama French paper
resize <- function(x) {
  return (matrix (x, nrow=5, byrow = TRUE, 
                  dimnames = list (c("SMALL", "2", "3", "4", "BIG"), c("LOW", "2", "3", "4", "HIGH"))))
}

# resize alpha
alpha <- resize(betas[1, ])
alpha

# resize beta
market.beta <- resize(betas[2, ])
SMB.beta <- resize(betas[3, ])
HML.beta <- resize(betas[4, ])

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

