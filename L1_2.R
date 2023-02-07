#link   https://rpubs.com/tijahbus/L1_2

library(matlib)

## Example 41
A <- matrix(c(1,2,1,1), nrow = 2, ncol = 2)
A

b <- c(4, 5)
b

Solve(A, b)

plotEqn(A,b)




## Example 42
A <- matrix(c(1,-2,-1,2,3,2,3,-2,1), nrow = 3, ncol = 3)
A

b <- c(6, -1, 2)
b

Solve(A, b)


solve(A, b)


plotEqn3d(A,b, xlim=c(0,4), ylim=c(0,4))






library(matlib)

## Example 44
A <- matrix(c(0, -1, 1, 0, 1, 1, 0, 1, 3, -4, 2, 0, -1, 0, 4, -4), 4, 4)
b <- c(1, 1, 5, -2)
showEqn(A, b)


echelon(A, b, verbose=TRUE, fractions=TRUE)


A <- matrix(c(0, -9, 4, 4, 5, 6, 3, 8, 4, 6, 0, 4, 7, 7, 5, 12), 4, 4)
b <- c(-9, -17, -3, 0)
showEqn(A, b)
Solve(A,b)

library(pracma)
Rank(A)
Rank(cbind(A,b))



A <- matrix(c(0, -9, 4, 4, 5, 6, 3, 8, 4, 6, 0, 4, 7, 7, 5, 12), 4, 4)
b <- c(-9, -17, -3, -12)
showEqn(A, b)
Solve(A,b)

Rank(A)
Rank(cbind(A,b))



B <- matrix(c(8,-9,1,10,
              -9,-7,-3,7,
              7,-25,-1,13,
              -1,-16,-2,3), nrow = 4, ncol = 4, byrow = TRUE)
b <- c(25,16,60,41)
echelon(cbind(B,b))

Solve(B,b)
solve(B,b)


