
## Given in description of exercise

library(ggplot2)

## DATA


lambda <- 0.2
n <- 40
sim <- 1000

## Exponential distribution
mn <- 1 / lambda
sd <- 1 / lambda

## MEAN



## mean of the population of 40 iid exponentials
data.frame(Estimated = c(mean(mns), var(mns)), Theoretical = c(mn, sd^2/n), 
          row.names = c("Mean", "Variance"))




#MEAN
g <- ggplot(data.frame(means = mns), aes(x = mns))
g <- g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.1, aes(y = ..density..))
g <- g + stat_function(fun = dnorm, size = 2, args = list(mean = mn, sd = sd/sqrt(n)))
g <- g + geom_vline(xintercept = mn, size = 2, colour = "red")
g <- g + labs(caption = paste("Figure ", 1), title = "Distribution of Simulated exponentials", y = "Density", x ="X")
g <- g + theme(plot.title = element_text(hjust=0.5))
g

rm(g)



dat <- data.frame(x = c(sim.vector, mns), test = c(rep("40000 simulations", sim * n), rep("1000 Means of 40 iid", sim)))

#MEAN TEST
g <- ggplot(dat, aes(x = x, fill = test))
g <- g + geom_histogram(alpha=.20, color = "black", fill = "lightblue", binwidth = 0.4, aes(y = ..density..))
g <- g + stat_function(fun = dnorm, size = 2, args = list(mean = mn, sd = sd/sqrt(n)), color ="lightgreen")
g <- g + geom_vline(xintercept = mn, size = 2, colour = "red")
g <- g + facet_grid(. ~ test)
g <- g + labs(caption = paste("Figure ", 1), title = "Distribution of Simulated exponentials", y = "Density", x ="X")
g <- g + theme(plot.title = element_text(hjust=0.5))
g <- g + xlim(3, 8)
g

##

