library(imager)
library(profvis)
profvis({
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
})

# code 2 


dog <- load.image("dog.jpeg")
dims = dim(dog)
rot = dog[dims[1]:1, dims[2]:1,]
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))




library(imager)
library(profvis)
profvis({
dog <- load.image("dog.jpeg")
dims = dim(dog)
rot = dog[dims[1]:1, dims[2]:1,]
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))
})


# Benchmark

benchmark({dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = dims)

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    rot[i, j, ] <- col.mat[dims[1] - i + 1, dims[2] - j + 1, ]
  }
}},
{
  dog <- load.image("dog.jpeg")
  dims = dim(dog)
  rot = dog[dims[1]:1, dims[2]:1,]
  par(mfrow = c(1,2))
  plot(dog)
  plot(as.cimg(rot))
})


