#link    https://rpubs.com/tijahbus/L2_1

### Library
library(magick)


inp_img <- image_read("http://polytopes.net/Tora_color.png")
image_info(inp_img)


plot(inp_img)


mod_img <- image_modulate(inp_img, brightness = 200, saturation = 820, hue = 100)
plot(mod_img)





library(mvtnorm)
library(ggplot2)
library(matlib)

## Standard deviation
sigma <- matrix(c(4,2,2,3), ncol = 2, nrow = 2) 

## Mean
mu <- c(1, 2)
n <- 1000
set.seed(123)
x <- rmvnorm(n = n, mean = mu, sigma = sigma)
d <- data.frame(x)
p2 <- ggplot(d, aes(x = X1, y = X2)) + geom_point(alpha = .5) + geom_density_2d()
p2


data <- data.frame(x)
theta <- 90
A %*% t(data[1,])

library(pracma)
contour_rotate = function(data, theta){

d <- data.frame(matrix(nrow = dim(data)[1], ncol = dim(data)[2]))
colnames(d) <- c("x.cor", "y.cor")
A <- matrix(c(cosd(theta), -sind(theta), sind(theta), cosd(theta)), nrow = 2, ncol = 2, byrow = TRUE)

for(i in 1:(dim(data)[1])){
  d[i,] <- t(A %*% t(data[i,]))
}

return(d)

}

p2 <- ggplot(contour_rotate(d,90), aes(x = x.cor, y = y.cor)) + geom_point(alpha = .5) + geom_density_2d()
p2







