# Causal Impact analysis HELLO WORLD ####
set.seed(1)

## define similar test (y) and control (x1) groups
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)

## introduce and 'interaction' at index 71
y[71:100] <- y[71:100] + 10

## define pre and post interaction periods
toy_pre.period <- c(1, 70)
toy_post.period <- c(71, 100)

library(CausalImpact)
library(ggplot2)

# Causal Impact wants a dataframe with the test group in the first column and 
# the control in the second
toy_data <- cbind(y, x1)

impact <- CausalImpact(toy_data, toy_pre.period, toy_post.period)
plot(impact) + ggtitle("Google Ads Group 1 vs Google Ads Group 2")
