# link   https://rpubs.com/tijahbus/L1_1

library(ISLR)
data(Smarket)
#Smarket
head(Smarket)

tail(Smarket)

dim(Smarket)

Smarket[2,]

M2 <- rbind(Smarket[3,], Smarket[4,], Smarket[7,], Smarket[9,], Smarket[10,])
M2

dim(Smarket)

x <- 1:1250
plot(x, Smarket[,2],type="l")

Smarket[1:242,2]
plot(x[1:242], Smarket[1:242,2],type="l")
