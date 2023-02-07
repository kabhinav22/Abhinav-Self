#link   https://rpubs.com/tijahbus/L4_2

library(pracma)
library(MASS)
A <- matrix(c(1, -1, 4, 2, 0, -1, -1, -1, 5), nrow=3, ncol=3, byrow=TRUE)
#A <- matrix(c(1, -1, 4, 1, 2, 0, -1, -2, -1, -1, 5, 3), nrow=3, ncol=4, byrow=TRUE)
rref(A)

Null(t(A))     # MASS
nullspace(A)   # pracma



t(rref(t(A))) # column space

orth(t(A)) # row space (as columns)


