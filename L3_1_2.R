#link   https://rpubs.com/tijahbus/L3_1

#library(conics)

##coefficients
#v <- c(2, 2, 2, -20, -28, 10)
#conicPlot(v)


library(plotly)

x <- c(4, 1, 3, 0)
y <- c(0, 2, 5, 3)
z <- c(0, 5, 1, 0)
intensity <- c(0, 0.33, 0.66, 1)
p<- plot_ly(x = x, y = y, z = z,
            type = "mesh3d",
            intensity = intensity,
            showscale = TRUE
)
p



library(matlib)
A <- matrix(c(0, 1, 3, -1, -1, 1, -4, 0, 1, 0, 2, 4, 0, 1, 0, -4), 
            nrow = 4, ncol = 4, byrow = TRUE)

AI <- rowadd(A, 1, 2, -2)
det(AI)

det(A)



AI <- rowmult(A, 1, -2)
det(AI)



s <-1:20
k <- 1/s
detAk <- numeric(20)
for(i in 1:20){
  detAk[i] <- det(matrix(c(k[i], k[i]^2, k[i]^3,
                         k[i]^2, k[i]^4, k[i]^6,
                         k[i]^3, k[i]^6, k[i]^9), nrow = 3, ncol = 3, byrow = TRUE))
}
plot(s,detAk, type = "l")





x <- c(2,0,1,4,6,5)
y <- c(2,0,-1,-1,-2,3)

area_sum <- 0
for(i in 2:(length(x)-1)){
  A <- matrix(c(2,2,1,x[i],y[i],1,x[i+1],y[i+1],1), nrow = 3, ncol = 3, byrow = TRUE)
  area_sum <- area_sum + (abs(det(A)))/2
  
}
area_sum





