###########################################
## Pseudo-random number generation
## using multiple congruential method
###########################################
m <- 2^31 - 1 # choose a value you want
a <- 7^5 # choose a value you want
x <- numeric(length = 1e3)
x[1] <- 7 #x0 -- choose
for(i in 2:1e3)
{
  x[i] <- (a * x[i-1]) %% m
}
# We will visualize using histograms (for testing uniformity)
# and trace plots (for checking independence)
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too





###########################################
## Pseudo-random number generation
## using mixed congruential method
###########################################
m <- 2^31 - 1 # choose a value you want
a <- 7^5 # choose a value you want
c <- 13
x <- numeric(length = 1e3)
x[1] <- 7 #x0 -- choose
for(i in 2:1e3)
{
  x[i] <- (c+(a * x[i-1])) %% m
}
# We will visualize using histograms (for testing uniformity)
# and trace plots (for checking independence)
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too



###########################################
## Accept Reject algorithm to draw from
## Binomial(n,p)
###########################################
# setting the seed makes it so that the same sets of
# random variables are realized.
set.seed(1)
# Function draws one value from Binom(n,p)
# n = number of trials
# p = probability of success
draw_binom <- function(n, p)
{
  accept <- 0 # Will track the acceptance
  try <- 0 # Will track the number of proposals
  # upper bound calculated in the notes
  x <- 0:n
  all_c <- choose(n,x) * (1-p)^(n - 2*x) * p^(x-1) # from notes
  c <- max(all_c) + .00001 # what is the value of c ?
  while(accept == 0)
  {
    try <- try + 1
    U <- runif(1)
    prop <- rgeom(1, prob = p) #draw proposal
    ratio <- dbinom(prop, size = n, prob= p)/(c* dgeom(prop, p))# calculate the ratio
    if(U < ratio)
    {
      accept <- 1
      rtn <- prop
    }
  }
  return(c(rtn, try))
}
draw_binom(n = 10, p = .25)


###
# If we want X1, ..., Xn ~ Binom(n.p)
# we need to call the function multiple times
# sample size
N <- 1e3
samp <- numeric(N)
n.try <- numeric(N)
for(t in 1:N)
{
  # I use as a dummy variable often
  foo <- draw_binom(n = 10, p = .25)
  samp[t] <- foo[1]
  n.try[t] <- foo[2]
}
mean(samp) #should be n*p = 2.5
mean(n.try)


# sample size
N <- 1e3 # reducing this
samp <- numeric(N)
n.try <- numeric(N)
for(t in 1:N)
{
  # I use as a dummy variable often
  foo <- draw_binom(n = 10, p = .50)
  samp[t] <- foo[1]
  n.try[t] <- foo[2]
}
mean(samp) #should be n*p = 5
mean(n.try)
# sample size
N <- 1e3 # reducing this
samp <- numeric(N)
n.try <- numeric(N)
for(t in 1:N)
{
  # I use as a dummy variable often
  foo <- draw_binom(n = 100, p = .25)
  samp[t] <- foo[1]
  n.try[t] <- foo[2]
}
mean(samp) #should be n*p = 25
mean(n.try) # when n is large the number of loops is too large!


plot(x, mass.geom, pch = 16, col = "red", type= "n")
points(mass.bin, pch = 16, col = "red", type= "h")
points(mass.geom, pch = 16, col = "blue", type = "h", lty = 2)





# Matching the means:
# choosing p* for rgeom so that np = (1-p*)/p*
p.star <- 1/(n*p + 1)
mass.geom <- dgeom(x, p.star)
mass.bin <- dbinom(x, size = n, prob = p)
all_c <- choose(n,x) * (1-p.star)^(n - 2*x) * p.star^(x-1)
(c <- max(all_c))
plot(mass.geom, pch = 16, col = "red", type= "n")
points(mass.bin, pch = 16, col = "red", type= "h")
points(mass.geom, pch = 16, col = "blue", type = "h", lty = 2)
       
       
?ppois
       
       



trunc_pois <- function(m = 20, lam = 20)
{
  accept <- 0
  try <- 0
  while(!accept)
  {
    try <- try + 1
    prop <- rpois(1, lambda = lam)
    if(prop <= m)
    {
      accept <- 1
    }
  }
  8
  return(c(prop, try))
}
N <- 1e3
out <- replicate(N, trunc_pois())
# out is 2 x 1000 matrix. First row are the samples
# second row are the number of loops
mean(out[1, ]) # mean of trunc pois
mean(out[2, ]) # mean of number loops, similar to c
hist(out[1, ], main = "Hist of Truncated Poisson")


x <- 0:5000

all_c <- dgeom(x, p = .10)/dpois(x, lambda = 10)
max(all_c)
hist(all_c)
















# using a box as a proposal
##############################
set.seed(1)
ellipse <- function(a,b)
{
  accept <- 0
  counter <- 0 # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    U1 <- runif(1)
    U2 <- runif(1)
    U11 <- -a + 2*a*U1
    U21 <- -b + 2*b*U2
    if(U11ˆ2/a + U21ˆ2/b <= 1) 
    {
      accept <- 1
      return(c(prop, counter))
    }
  }
}
