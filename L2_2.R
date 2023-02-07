#link   https://rpubs.com/tijahbus/L2_2

library(matlib)
A <- matrix(c(1,-2,-1,2,3,2,3,-2,1), nrow = 3, ncol = 3)
inv(A)

library(MASS)
fractions(inv(A))

solve(A)
