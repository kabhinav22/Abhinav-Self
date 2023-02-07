#link    https://rpubs.com/tijahbus/L2_3

A <- matrix(c(1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 ,
              1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 
              0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 
              0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 
              0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 
              0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 
              0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1), nrow = 11, ncol = 18, byrow = TRUE)
b <- c(1298, 1948, 465 , 605 , 451 ,  338 , 260 ,  183 ,  282 ,  127 ,  535)
library(pracma)
rref(A)

C <- cbind(A, b)
E <- rref(C)
E <- E[-11,]

G1 <- eye(8)
G2 <- matrix(rep(0, 80), 8, 10)
b2 <- c(266, 223, 140, 264, 137, 67, 130, 24)
G <- cbind(G1, G2, b2)
M <- rbind(E, G)
inv(M[,-19])

inv(M[, -19]) %*% M[, 19]






M2 <- cbind(M[,-19],diag(18)) 
library(pracma)
M3 <- rref(M2)
Minv <- M3[,19:36]
Minv %*% M[,19]



library(png)
### You need download from http://polytopes.net/Tora_color.png and store the directory currently you are working on.
x <- readPNG("Tora_color.png")
dim(x)

library(magick)

inp_img <- image_read("http://polytopes.net/Tora_color.png")
image_info(inp_img)

plot(inp_img)

image_flip(inp_img)
image_flop(inp_img)

mod_img <- image_rotate(inp_img, 180)
plot(mod_img)
