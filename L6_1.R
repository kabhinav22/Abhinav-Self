#link  https://rpubs.com/tijahbus/L6_1

## library need to open a jpg file
library(jpeg)
## Download from the author's website
myurl <- "http://polytopes.net/Tora_Sleeping.JPG"
z <- tempfile()
download.file(myurl,z,mode="wb")
Kitty <- readJPEG(z)
## dimension
d <- dim(Kitty)
r <- Kitty[,,1]
g <- Kitty[,,2]
b <- Kitty[,,3]

kitty.r.pca <- prcomp(r, center = FALSE)
kitty.g.pca <- prcomp(g, center = FALSE)
kitty.b.pca <- prcomp(b, center = FALSE)

View(kitty.r.pca$x)



kitty.rgb.pca <- list(kitty.r.pca, kitty.g.pca, kitty.b.pca)

num <- c(2, 50, 100, 1000, 2000)

for (i in num) {
  pca.img <- sapply(kitty.rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('tora_compressed_', 
                           round(i,0), '_components.jpg', sep = ''))
}




library(jpeg)

tour = readJPEG("paris_tourist.jpg")
no_tour = readJPEG("paris_no_tourist.jpg")
new = readJPEG("rome.jpg")
dim(tour); dim(no_tour); dim(new)

rno_tour = no_tour[, ,1]
gno_tour = no_tour[, ,2]
bno_tour = no_tour[, ,3]
no_tour_rgb = list(rno_tour,gno_tour,bno_tour)

prtour = prcomp(rno_tour, center = FALSE)
pgtour = prcomp(gno_tour, center = FALSE)
pbtour = prcomp(bno_tour, center = FALSE)

tour.pca = list(prtour, pgtour, pbtour)

library(dplyr)
# Create a dataframe for easier plotting
df = data.frame(scheme = rep(c("R", "G", "B"), each = 458), index = rep(1:458,  3), var = c(prtour$sdev^2, pgtour$sdev^2, pbtour$sdev^2))

View(df)

df %<>% group_by(scheme) %>%
  mutate(propvar = 100*var/sum(var)) %>%
  mutate(cumsum = cumsum(propvar)) %>%
  ungroup()


# scree plot
library(ggplot2)
#relevel to make it look nicer
#df$scheme = factor(df$scheme,levels(df$scheme)[c(3,2,1)])

df %>% ggplot(aes(x = index, y = propvar, fill = scheme)) + 
  geom_bar(stat="identity") + 
  labs(title="Screeplot of Principal Component", x ="Principal Component", 
       y="% of Variance") + geom_line()  + 
  scale_x_continuous(limits = c(0,30)) +
  facet_wrap(~scheme)




df %>% ggplot(aes( x = index, y = cumsum, fill = scheme)) + 
  geom_bar(stat="identity") + 
  labs(title="Cumulative Proportion of Variance Explained Principal Component", x="Principal Component", 
       y="Cumulative % of Variance") + geom_line() + 
  scale_x_continuous(limits = c(0,30)) +
  facet_wrap(~scheme)



################################################################
# This is the number of desired PCs
#pcnum = c(2,30,200, 300)
pcnum = (1:5)*10

compress <- function(trained_rgb_pca, newrgb, pcnum, dims){
  r_rotate = trained_rgb_pca[[1]]$rotation
  g_rotate = trained_rgb_pca[[2]]$rotation
  b_rotate = trained_rgb_pca[[3]]$rotation
  
  r = newrgb[[1]]
  g = newrgb[[2]]
  b = newrgb[[3]]
  
  pred_r = (r %*% r_rotate)[,1:pcnum] %*% t(r_rotate[,1:pcnum])
  pred_g = (g %*% g_rotate)[,1:pcnum] %*% t(g_rotate[,1:pcnum])
  pred_b = (b %*% b_rotate)[,1:pcnum] %*% t(b_rotate[,1:pcnum])
  
  pred.pca = list(pred_r, pred_g, pred_b)
  pred.array = array(as.numeric(unlist(pred.pca)),dim = dims)
  return(pred.array)
}

for(i in pcnum){
  pca.img = compress(tour.pca, no_tour_rgb, pcnum = i, dims = c( 458, 715,3))
  writeJPEG(pca.img, paste('no_tourist_compressed_', 
                           round(i,0), '_components.jpg', sep = ''))
}

## compressed img of paris tourist
tour = readJPEG("paris_tourist.jpg")
dim(tour);

rtour = tour[, ,1]
gtour = tour[, ,2]
btour = tour[, ,3]
tour_rgb = list(rtour,gtour,btour)

prtour = prcomp(rtour, center = FALSE)
pgtour = prcomp(gtour, center = FALSE)
pbtour = prcomp(btour, center = FALSE)

tour.pca = list(prtour, pgtour, pbtour)



for(i in pcnum){
  pca.img = compress(tour.pca, tour_rgb, pcnum = i, dims = c( 459, 715,3))
  writeJPEG(pca.img, paste('tourist_compressed_', 
                           round(i,0), '_components.jpg', sep = ''))
}



## compressed img of rome
new = readJPEG("rome.jpg")
dim(new)

rnew = new[, ,1]
gnew = new[, ,2]
bnew = new[, ,3]
new_rgb = list(rnew,gnew,bnew)

prnew = prcomp(rnew, center = FALSE)
pgnew = prcomp(gnew, center = FALSE)
pbnew = prcomp(bnew, center = FALSE)

tour.pca = list(prnew, pgnew, pbnew)


for(i in pcnum){
  pca.img = compress(tour.pca, new_rgb, pcnum = i, dims = c( 500, 750,3))
  writeJPEG(pca.img, paste('rome_compressed_', 
                           round(i,0), '_components.jpg', sep = ''))
}










library(mvtnorm)
library(ggplot2)
library(matlib)
## Standard deviation
sigma <- matrix(c(4,3,3,4), ncol = 2, nrow = 2) 
## Mean
mu <- c(5, 5)
n <- 1000
set.seed(123)
x <- rmvnorm(n = n, mean = mu, sigma = sigma)
d <- data.frame(x)
p2 <- ggplot(d, aes(x = X1, y = X2)) + geom_point(alpha = .5) + geom_density_2d()
p2


A <- matrix(c(2, 1, 1, 2), 2, 2)
eigen(A)


### Practical Applications
A <- matrix(c(1/4, 1/5, 1/3, 1/6, 1/4, 1/5, 1/3, 1/3, 1/4, 2/5, 1/6, 1/3, 1/4, 1/5, 1/6, 1/6), 4, 4)
p <- eigen(A)$vectors
D <- diag(eigen(A)$values)
p %*% D %*% solve(p)
