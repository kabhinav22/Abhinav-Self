#link    https://rpubs.com/tijahbus/L4_4

### Practical Applications

## library
library(mvtnorm)
## setting a seed to reproduce the same result
set.seed(123)
sigma <- matrix(c(4,2,2,3), ncol=2)
x <- rmvnorm(n=500, mean=c(0,0), sigma=sigma)

pca <- princomp(x)
pca$loadings

T <- pca$loadings
D <- inv(T) %*% t(x)
y <- t(D)
plot(y)





library(ISLR)

## Libraries for Plotting our Results
library(ggplot2)
library(ggfortify)
library(gridExtra)

## for inv() function
library(pracma)

## Loading the data
#   data(Credit)
dim(Default)
credit.data <- Default[,3:4]
summary(credit.data)
Credit
Default
head(Default)

library(e1071)
z <- svm(default ~ ., Default[,c(1,3:4)])
plot(z, Default[,c(1,3:4)])





library(ISLR)
data(Auto)
dim(Auto)

auto <- Auto[,1:7]
summary(auto)
head(auto)
new.data <- Auto[, 1:8]
new.data$origin <- as.character(new.data$origin)
summary(new.data)
### Practical Applications
pc <- princomp(auto)
plot(pc)
T <- pc$loadings
T_star <- T[,1:2]

plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))

T_comp <- t(t(T_star) %*% t(auto))
dat <- data.frame(Auto$origin,T_comp)
colnames(dat) <- c("origin","comp1","comp2")
View(dat)

library(e1071)
attach(dat)
z <- svm(origin ~ ., dat, type = "C-classification")
plot(z, dat)

