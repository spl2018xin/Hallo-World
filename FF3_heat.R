library(ggplot2)

# Calculate 25 portfolio returns and std
# Inserted after unlist() of Rf
P25.return <- colMeans(P25[,-1]-FF3$RF)
P25.std <- apply(P25[,-1]-FF3$RF, 2, sd)

resize <- function(x)
{
  df <- data.frame(matrix(as.numeric(x), nrow=5, byrow = TRUE))
  colnames(df) <- c("LOW", "2", "3", "4", "HIGH")
  rownames(df) <- c("SMALL", "2", "3", "4", "BIG")
  return(df)
}

heat.prep = function(df)
{
  df.return = expand.grid(HML = c("LOW", "2", "3", "4", "HIGH"),
                           SMB = c("SMALL", "2", "3", "4", "BIG"))
  df.return$Return = df
  return(df.return)
}


heat.plot = function(df, legend.label = "Return")
{
  # Heat-map
  # Important: + sign at the end of the line, not beginning.

    ggplot(data = heat.prep(df), aes(x = HML, y = SMB, fill = Return)) + 
    geom_tile() + geom_text(aes(label=round(Return, digits = 2))) + 
    scale_y_discrete(limits = rev(levels(heat.prep(P25.return)$SMB))) + 
    labs(fill = legend.label)
}

resize(heat.prep(P25.return)[,3])

heat.plot(P25.return)

# P25.return is different from the results in the paper
# which explains why we get different results in replicating

heat.plot(betas[1,],"Alpha")
heat.plot(betas[2,],"Market Beta")
heat.plot(betas[3,],"SMB Beta")
heat.plot(betas[4,],"HML Beta")

Alpha <- betas[1,]
Market.beta <- betas[2,]
SMB.beta <- betas[3,]
HML.beta <- betas[4,]
y <- lm(P25.return ~ Alpha + Market.beta + SMB.beta + HML.beta)
summary(y)
plot(predict(y),P25.return,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)
