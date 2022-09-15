# Worksheet 1

# 1

abhi = function(n)
{
  count = 1
  for (i in 1:n)
  {
    count = count * i
  }
  return(count)
}



# 2

euler = function(n)
{
  nlim <- (1 + 1/n)^(n)
  return(nlim)
}



# 3

seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
seat[seat$Roll == 221332, ]
seat[seat$Roll == 221252, seat$Seat == "ESCPC112"] ??
seat[ , seat$Seat == "ESCPC112"] ??


# 4

seat <- read.csv("seating.csv") ?



# Worksheett 2

# 1

seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
msc1 = seat[seat$Roll > 220000, ]
msc2 = seat[seat$Roll < 220000, ]
msc1
msc2
length(msc1$Roll)
length(msc2$Roll)


# 2

cricket <- read.csv("https://dvats.github.io/assets/course/mth208/battingbowling.csv")
all_rounder = cricket[cricket$Batting > 25 & cricket$Bowling < 40 , ]
all_rounder
length(all_rounder$Batting)
table(all_rounder$Team)
table(cricket$Team) 


# New Zealand has most all_rounders & South Africa has least



# 3

plot(x = 1:10, y = 1:10, type = "l", xlab = "x", ylab = "y", main = "Y = X Plot")
abline(h = 5, v = 5, col = "red")


# 
square = function(n)
{
  y = n^2
  return(y)
}

x = square 
n = 1:10
plot(x,y) ?
plot(x,y, type = "l" , xlab = "x" , ylab = "y", main = "Y = X^2 Plot")
abline(h = 25 , v = 25 , col = "red")





# 4

fx <- function(n)
{
  rtn <- (1 + 1/n)^n
  return(rtn)
}

n = 1:1000
fn = fx(n)

plot(n, fn, type = "l")  
abline(h = exp(1), col = "red")



# Worksheett 3

# 1

tosses = rbinom(n = 1000, size = 1, prob = 0.5)
prop = sum(tosses)/1000

tosses = rbinom(n = 1000, size = 1, prob = 0.30)
prop = sum(tosses)/1000

tosses = rbinom(n = 1000, size = 10, prob = 0.30)
tosses
prop = sum(tosses)/1000

# 2

sample(1:3, size = 1, prob = c(3/7,2/7,2/7))
sample(1:3, size = 1, prob = c(3,2,2)/7)


A = matrix(c(3, 1, -2, 4, 5, 3, -1, 2, -2), nrow = 3, ncol = 3)

p_vec = numeric(length = 3)
for(k in 1:ncol(A))
{
  p_vec[k] = norm(A[ ,k], type = "2")
}
p_vec = p_vec/sum(p_vec)
sample(1:3, size = 1, prob = p_vec)



B = matrix(c(4,2,5,6,2,7,8,9,10,1,0,5), nrow = 3, ncol = 4)
B
p_vec = numeric(length = 3)
for (i in 1:nrow(B))
{
  p_vec[i] = norm(B[i, ], type = "2")
}
p_vec[1]
p_vec[2]
p_vec[3]
p_vec = p_vec/sum(p_vec)
p_vec 

sample(1:3, size = 1, prob = p_vec)



runif(n = 10 , min = 0, max = 5)


# 3

attempts = function()
{
  sum = 0
  count = 0
  while(sum <= 1)
  {
    sum = sum + runif(1, min = 0, max = 1)
    count = count + 1
  }
  return(count)
}
attempts()


new = numeric(length = 1000)
for (i in 1:1000) 
{
  new[i]= attempts()
}
new

mean(new)


# 4

attempts = function(age)
{
  count = 0
  remain = age
  while (remain > 0)
  {
    count = count + 1
    blow_out = sample(1:remain , size = 1)
    remain = remain - blow_out
  }
  return(count)
}
attempts(25)  

new = numeric(length = 1000)
for (i in 1:1000)
{
  new[i] = attempts(25)
}
new

mean(new)




new = numeric(length = 1000)
for (i in 1:1000)
{
  new[i] = attempts(30)
}
new

mean(new)





# Assignment 1


tennis = function(p)
{
  A = 0
  B = 0
  for(k in 1:5)
  {
    next.game = sample(0:1, size = 1, prob = c(p, 1-p))
    if(next.game == 0) B = B+1
    if(next.game == 1) A = A+1
    
    if(A == 3 || B == 3)
    {
      x = k 
      break
    }
  } 
  return(x)
}


matches = numeric(length = 1000)
for (i in 1:1000)
  {
  matches[i] = tennis(0.70)
}
matches

ans = mean(matches)



# Worksheet 4

install.packages("imager")
install.packages("tidyverse")
install.packages("rvest")

library(imager)
dog = load.image("dog.jpeg")
plot(dog)
dim(dog)
graydog = grayscale(dog)
plot(graydog)
dim(graydog)
gray.mat <- as.matrix(graydog[,,1,1])
dim(gray.mat)
col.mat <- as.array(dog[, ,1, ])
dim(col.mat)
cropped.mat <- col.mat[1:300, , ]
crop.dog <- as.cimg(cropped.mat)
plot(crop.dog)



dog = load.image("C:/Users/abhin/Downloads/dog.jpeg")
plot(dog)
dim(dog)

land1 = load.image("C:/Users/abhin/Downloads/land1.jpeg")
plot(land1)
dim(land1)


getwd()
land2 = load.image("land2.jpeg")
plot(land2)
dim(land2)





land1 <- load.image("land1.jpeg")
land2 <- load.image("land2.jpeg")

# We can reuse function diff.col!
# measuring average distance to c(1,1,1) for both pics
distance.to.white1 <- mean(diff.col(land1, c(1,1,1)))
distance.to.white2 <- mean(diff.col(land2, c(1,1,1)))

ifelse(distance.to.white1 < distance.to.white2, "Land1", "Land2")







dog <- load.image("dog.jpeg")
col.mat<-as.array(dog[, ,1, ])

dims <- dim(col.mat)

# Calculate distance to purest color
# for each pixel
dist <- matrix(0, nrow = dims[1], ncol = dims[2])
for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # replace c(0,1,0) with c(1,0,0) or c(0,0,1) for other problems
    dist[i,j] <- norm(col.mat[i,j, ] - c(0,1,0), "2")
  }
}

# find pixels with smallest distance
ind1 <- which(dist == min(dist), arr.ind = TRUE)

# another way
# read ?apply help page to understand the command below
dist <- apply(col.mat, c(1,2), function(s) norm(s - c(0,1,0), "2"))
ind2 <- which(dist == min(dist), arr.ind = TRUE)

# checking to see that ind1 = ind2. Hence both methods are same
ind1 == ind2

plot(dog)
points(ind1, col = "red")





###################################
# Problem 3

col1 <- load.image("col1.png")
# col1 has 4 color channels for some reason. 
# Removing the last one.
col1 <- as.cimg(col1[ , , , 1:3])

col2 <- load.image("col2.png")
col3 <- load.image("col3.png")


# I will make a function that calculates the
# difference matrix from a color for each image

diff.col <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # distance from the col give by user
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  # return the distance matrix of each pixel
  return(dist)
}

## Making another function for each color 
## for a given image. Then for whichever primary
## color the mean of the distance matrix is smallest
## That will be the guess. 
which.color <- function(img)
{
  dist.cols <- numeric(length = 3)
  
  # make a matrix of ones.
  # this is a shortcut to define all three
  # primary colors in one go.
  colors.matrix <- diag(3)  
  for(k in 1:3)
  {
    # picking kth column of colors.matrix as we are picking
    # the kth primary color to compare to
    dist.cols[k] <- mean(diff.col(img, col = colors.matrix[ ,k]))
  }
  
  # which color has the smallest distance
  ind <- which.min(dist.cols)
  guess <- c("red", "green", "blue")[ind]
  
  return(guess)
}

which.color(col1)  # guessing for col1
which.color(col2)  # guessing for col2
which.color(col3)  # guessing for col3









#######################################
## Worksheet 5 Solutions ##############
#######################################

library(imager)

##############################
# Problem 1
# 180 deg means that n x m remains n x m
# and directions are flipped
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = dims)

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    rot[i, j, ] <- col.mat[dims[1] - i + 1, dims[2] - j + 1, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))



##############################
# Problem 2
# 90 deg means that n x m becomes m x n
# and then appropriate rotation
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = c(dims[2], dims[1], dims[3]))

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # i component becomes j
    rot[j, i, ] <- col.mat[i, dims[2] - j + 1, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))




##############################
# Problem 3
# 90 deg means that n x m becomes m x n
# and then appropriate rotation
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = c(dims[2], dims[1], dims[3]))

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # i component becomes j
    rot[j, i, ] <- col.mat[dims[1] - i + 1, j, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))




##############################
# Problem 4
# 90 deg means that n x m becomes m x n
# and then appropriate rotation
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = c(dims[2], dims[1], dims[3]))

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # i component becomes j
    rot[j, i, ] <- col.mat[dims[1] - i + 1, j, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))




##############################
# Problem 5

# cropping image first
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])
col.mat <- col.mat[1:600, 1:600, ]

reduce <- array(0, dim = c(300, 300, 3))
red.dims <- dim(reduce)

# I will take an average of the 2 by 2 pixels
# and then assign the average value to the reduce
averaging <- rep(0,3)
for(i in 1:red.dims[1])
{
  for(j in 1:red.dims[2])
  {
    ind1 <- (2*(i-1) + 1): (2*i)
    ind2 <- (2*(j-1) + 1): (2*j)
    
    # taking the average rbg in the 2 by 2 area
    averaging[1] <- mean(col.mat[ind1, ind2, 1])
    averaging[2] <- mean(col.mat[ind1, ind2, 2])
    averaging[3] <- mean(col.mat[ind1, ind2, 3])
    
    reduce[i, j, ] <- averaging
  }
}
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(reduce))
save.image(as.cimg(reduce), file = "dog_300.jpeg")


##############################
# Problem 5

# cropping image first
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])
col.mat <- col.mat[1:600, 1:600, ]

reduce <- array(0, dim = c(60, 60, 3))
red.dims <- dim(reduce)

# I will take an average of the 10 by 10 pixels
# and then assign the average value to the reduce
averaging <- rep(0,3)
for(i in 1:red.dims[1])
{
  for(j in 1:red.dims[2])
  {
    ind1 <- (10*(i-1) + 1): (10*i)
    ind2 <- (10*(j-1) + 1): (10*j)
    
    # taking the average rbg in the 10 by 10 area
    averaging[1] <- mean(col.mat[ind1, ind2, 1])
    averaging[2] <- mean(col.mat[ind1, ind2, 2])
    averaging[3] <- mean(col.mat[ind1, ind2, 3])
    
    reduce[i, j, ] <- averaging
  }
}
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(reduce))
save.image(as.cimg(reduce), file = "dog_60.jpeg")
