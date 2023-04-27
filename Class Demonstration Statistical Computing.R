

#### Lab 6 Demonstration

###############
# Demo on Ridge Regression 
###############

set.seed(1)
n <- 50
p <- 1
beta.star <- c(4)
beta.star  # to output

# Making design matrix, first column is 1
X <- matrix(rnorm(n*p), nrow = n, ncol = p)

# Generating response
y <-  X %*% beta.star + rnorm(n, mean = 0, sd = 1)

# -ve log likelihood
negLogLik <- function(y, X, beta)
{
  t(y - X %*% beta) %*% (y - X %*% beta)/2
}

ridgePen <- function(beta, lam)
{
  lam * t(beta) %*% beta/2
}

grid <- 5e2
betas <- seq(-2, 10, length = grid)
nll <- numeric(length = 5e2)
pen <- numeric(length = 5e2)
ridge <- numeric(length = 5e2)
lam <- 10
for(i in 1:grid)
{
  nll[i] <- negLogLik(y, X, beta = betas[i])
  pen[i] <- ridgePen(beta = betas[i], lam = lam)
  ridge[i] <- nll[i] + pen[i]
}

plot(betas, nll, type = 'l', ylim = range(c(nll, ridge, pen)),
     xlab = expression(beta), ylab = "Objective function")
points(x = betas[which.min(nll)], y = min(nll), pch = 16)
lines(betas, pen, col = "red")
lines(betas, ridge, col = "blue")
points(x = betas[which.min(ridge)], y = min(ridge), pch = 16, col = "blue")
legend("topleft", col = c("black", "red", "blue"), lty = 1,
       legend = c("Neg Log Likelihood", "Penaltry term", "Ridge Objective Fn"))





####################################################
### MLE for Gamma(alpha, 1)
####################################################
set.seed(100)
library(pracma)  #for psi function

####################################################
# riginal data sample size is small first
# The NR methods estimates the MLE. Here the 
# blue and red lines will not match because
# the data is not large enough for the consistency of 
# the MLE to kick in.


alpha <- 5 #true value of alpha
n <- 10 # actual data size is small first
dat <- rgamma(n, shape = alpha, rate = 1)

alpha_newton <- numeric()
epsilon <- 1e-8  #some tolerance level preset
alpha_newton[1] <- 2  #alpha_0
count <- 1
tol <- 100 # large number
while(tol > epsilon)
{
  count <- count + 1
  
  #first derivative
  f.prime <- -n*psi(k = 0, alpha_newton[count - 1]) + sum(log(dat))
  
  #second derivative
  f.dprime <- -n*psi(k = 1, alpha_newton[count - 1])
  alpha_newton[count] <- alpha_newton[count - 1] - f.prime/f.dprime
  tol <- abs(alpha_newton[count] - alpha_newton[count-1])
}
alpha_newton

#Plot the log.likelihood for different values of alpha
alpha.grid <- seq(0, 10, length = 100)
log.like <- numeric(length = 100)
for(i in 1:100)
{
  log.like[i] <- sum(dgamma(dat, shape = alpha.grid[i], log = TRUE))
}
plot(alpha.grid, log.like, type = 'l', xlab = expression(alpha), ylab = "Log Likelihood")
abline(v = alpha, col = "red", lty = 2)
for(t in 1:count)
{
  points(alpha_newton[t], sum(dgamma(dat, shape = alpha_newton[t], log = TRUE)), pch = 16)
}
abline(v = tail(alpha_newton[count]), col = "blue", lty = 2)
legend("bottomright", legend = c("Likelihood", "Truth", "MLE"), lty = c(1,2,2), col = c("black", "red", "blue"))






####################################################
# Increasing original data sample size.
# Now the MLE is closer to the "truth"
# and our NR method obtains the MLE.
# Blue and red lines should match a lot

# Randomly generate data
alpha <- 5 #true value of alpha
n <- 1000 # actual data size is small first
dat <- rgamma(n, shape = alpha, rate = 1)
alpha_newton <- numeric()
epsilon <- 1e-8  #some tolerance level preset
alpha_newton[1] <- 2  #alpha_0
count <- 1
tol <- 100 # large number
while(tol > epsilon)
{
  count <- count + 1
  f.prime <- -n*psi(k = 0, alpha_newton[count - 1]) + sum(log(dat))
  f.dprime <- -n*psi(k = 1, alpha_newton[count - 1])
  alpha_newton[count] <- alpha_newton[count - 1] - f.prime/f.dprime
  tol <- abs(alpha_newton[count] - alpha_newton[count-1])
}
alpha_newton

#Plot the log.likelihood for different values of alpha
alpha.grid <- seq(0, 10, length = 100)
log.like <- numeric(length = 100)
for(i in 1:100)
{
  log.like[i] <- sum(dgamma(dat, shape = alpha.grid[i], log = TRUE))
}
plot(alpha.grid, log.like, type = 'l', xlab = expression(alpha), ylab = "Log Likelihood")
abline(v = alpha, col = "red", lty = 2)
for(t in 1:count)
{
  points(alpha_newton[t], sum(dgamma(dat, shape = alpha_newton[t], log = TRUE)), pch = 16)
}
abline(v = tail(alpha_newton[count]), col = "blue", lty = 2)
legend("bottomright", legend = c("Likelihood", "Truth", "MLE"), lty = c(1,2,2), col = c("black", "red", "blue")) 



#### Lab 7 Class Demonstration 

################################################
## MLE for location Cauchy distribution using
## Newton-Raphson method
## We will plot the likelihood as well
################################################
set.seed(1)
mu.star <- 5  # Setting true mu
n <- 4  # sample size
X <- rt(n, df = 1) + mu.star

## Function calculates  the log-likelihood
log.like <- function(mu, X)
{
  n <- length(X)
  rtn <- -n*log(pi) - sum( log(1 + (X - mu)^2) )
  return(rtn)
}

mu.x <- seq(-10, 40, length = 1e3)  # A sequence of mu's 
ll.est <- sapply(mu.x, log.like, X)  # evaluating log-likelihood at the mus
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))  # plotting log-likelihood. Not concave, so we need to choose good starting values.


## Starting Newton-Raphson method
tol <- 1e-5  # tolerance level for when to stop algorithm


## Returns derivate of log-likelihood
f.prime <- function(X, mu)
{
  rtn <- sum(2* (X - mu)/(1 + (X-mu)^2))  #f.prime
  return(rtn)
}

# Returns double derivative of log-likelihood.
f.double.prime <- function(X, mu)
{
  rtn <- sum( 2 * ( 2*(X-mu)^2/ (1 + (X - mu)^2)^2  - (1 + (X-mu)^2)^(-1) )  )
  return(rtn)
}

## Loop below stops when |mu_(k+1) - mu_(k)| < tol

current <- median(X)  # Good starting value
diff <- 100  # inital large value for difference
iter <- 0    # counting the number of iterations

mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current - f.prime(X, current)/f.double.prime(X, current)  # NR update
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(mu.k, evals, pch = 16, col = rgb(0,0,1, alpha = .5))


## Loop below stops when |mu_(k+1) - mu_(k)| < tol
current <- 7  # Bad starting value
diff <- 100
iter <- 0
mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current - f.prime(X, current)/f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(1,0,0, alpha = .5))

current <- 19  # Worst starting value
diff <- 100
iter <- 0
mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current - f.prime(X, current)/f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(.2,.7,.1, alpha = .8))

legend("topright", legend = c("Good starting", "Bad starting", "Horrible starting"), pch = 16, col = c("blue", "red", rgb(.2,.7,.1)))






###########################################
### Repeating again with a different seed
###########################################
set.seed(10)

mu.star <- 5  # Setting true mu
n <- 4  # sample size
X <- rt(n, df = 1) + mu.star

## Function calculates  the log-likelihood
log.like <- function(mu, X)
{
  n <- length(X)
  rtn <- -n*log(pi) - sum( log(1 + (X - mu)^2) )
  return(rtn)
}

mu.x <- seq(-10, 40, length = 1e3)  # A sequence of mu's 
ll.est <- sapply(mu.x, log.like, X)  # evaluating log-likelihood at the mus
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))  # plotting log-likelihood. Not concave, so we need to choose good starting values.


## Starting Newton-Raphson method
tol <- 1e-5  # tolerance level for when to stop algorithm

## Loop below stops when |mu_(k+1) - mu_(k)| < tol

current <- median(X)  # Good starting value
diff <- 100  # inital large value for difference
iter <- 0    # counting the number of iterations

mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current - f.prime(X, current)/f.double.prime(X, current)  # NR update
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(mu.k, evals, pch = 16, col = rgb(0,0,1, alpha = .5))


## Loop below stops when |mu_(k+1) - mu_(k)| < tol
current <- 2  
diff <- 100
iter <- 0
mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current - f.prime(X, current)/f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(1,0,0, alpha = .5))

current <- 19  # Bad starting value
diff <- 100
iter <- 0
mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current - f.prime(X, current)/f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(.2,.7,.1, alpha = .8))

## The problem here is that the sample size is low, so that the median and the MLE both are bad estimators.








###########################################
### Repeating with same seed but larger sample size
###########################################
set.seed(10)

mu.star <- 5  # Setting true mu
n <- 1e5  # sample size
X <- rt(n, df = 1) + mu.star

## Function calculates  the log-likelihood
log.like <- function(mu, X)
{
  n <- length(X)
  rtn <- -n*log(pi) - sum( log(1 + (X - mu)^2) )
  return(rtn)
}

mu.x <- seq(-10, 40, length = 1e3)  # A sequence of mu's 
ll.est <- sapply(mu.x, log.like, X)  # evaluating log-likelihood at the mus
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))  # plotting log-likelihood. Not concave, so we need to choose good starting values.


## Starting Newton-Raphson method
tol <- 1e-5  # tolerance level for when to stop algorithm

## Loop below stops when |mu_(k+1) - mu_(k)| < tol

current <- median(X)  # Good starting value
diff <- 100  # inital large value for difference
iter <- 0    # counting the number of iterations

mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current - f.prime(X, current)/f.double.prime(X, current)  # NR update
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(mu.k, evals, pch = 16, col = rgb(0,0,1, alpha = .5))


## Loop below stops when |mu_(k+1) - mu_(k)| < tol
current <- 3.5    # Very large jump!
diff <- 100
iter <- 0
mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current - f.prime(X, current)/f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(1,0,0, alpha = .5))

## The problem here is that the jumps are very large, so it's better to use Modified Newton-Raphson (see problem set)




################################################
## MLE for location Cauchy distribution using
## Gradient Ascent method
## We will plot the likelihood as well
################################################
set.seed(1)
mu.star <- 5  # Setting true mu
n <- 4  # sample size
X <- rt(n, df = 1) + mu.star

## Function calculates  the log-likelihood
log.like <- function(mu, X)
{
  n <- length(X)
  rtn <- -n*log(pi) - sum( log(1 + (X - mu)^2) )
  return(rtn)
}

mu.x <- seq(-10, 40, length = 1e3)  # A sequence of mu's 
ll.est <- sapply(mu.x, log.like, X)  # evaluating log-likelihood at the mus
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))  # plotting log-likelihood. Not concave, so we need to choose good starting values.


## Starting Gradient-Ascent method
tol <- 1e-5  # tolerance level for when to stop algorithm


## Returns derivate of log-likelihood
f.prime <- function(X, mu)
{
  rtn <- sum(2* (X - mu)/(1 + (X-mu)^2))  #f.prime
  return(rtn)
}

## Loop below stops when |mu_(k+1) - mu_(k)| < tol
t <- .3  # change this to 1 and see what happens to the "bad" starting value



## Loop below stops when |mu_(k+1) - mu_(k)| < tol
current <- 7  # Bad starting value
diff <- 100
iter <- 0
mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current + t*f.prime(X, current) 
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(mu.k, evals, pch = 16, col = rgb(1,0,0, alpha = .5))


current <- median(X)  # Good starting value
diff <- 100  # inital large value for difference
iter <- 0    # counting the number of iterations

mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current + t*f.prime(X, current)  # GD update
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(0,0,1, alpha = .5))


current <- 19  # Worst starting value
diff <- 100
iter <- 0
mu.k <- current
while( (diff > tol) && iter < 100)
{
  iter <- iter + 1
  update <- current + t*f.prime(X, current) 
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(.2,.7,.1, alpha = .8))

legend("topright", legend = c("Good starting", "Bad starting", "Horrible starting"), pch = 16, col = c("blue", "red", rgb(.2,.7,.1)))





################################################
## MLE for logistic regression
## Using gradient ascent
################################################
library(mcmc) #to load a dataset
data(logit)
head(logit)  # y is response and 4 covariates


y <- logit$y
X <- as.matrix(logit[, 2:5])
p <- dim(X)[2]

f.gradient <- function(y, X, beta)
{
  beta <- matrix(beta, ncol = 1)
  p_i <- exp(X %*% beta) / (1 + exp(X%*%beta))  
  rtn <- colSums(X* as.numeric(y - p_i))
  return(rtn)
}

store.beta <- matrix(0, nrow = 1, ncol = p)
store.grad <- matrix(0, nrow = 1, ncol = p)
beta_k <- rep(0, p) # start at all 0s
grads_norm <- 100 # large values
t <- .1
tol <- 1e-10
iter <- 0
while((grads_norm > tol) && iter < 1e4)  #not too many iterations
{
  iter <- iter+1
  old <- beta_k
  grad_k <- f.gradient(y = y, X= X, beta = old)
  grads_norm <- sum(grad_k^2)
  beta_k = old + t* grad_k
  store.beta <- rbind(store.beta, beta_k)
  store.grad <- rbind(store.grad, grad_k)
}
iter 
beta_k # last estimate
plot.ts(apply(store.grad, 1, norm, "2"), ylab = "Norm of Gradient")




### Lab 8 Class Demonstration


################################################
## MLE for location Cauchy distribution using
## MM-algorithm
################################################
set.seed(1)
mu.star <- 5  # Setting true mu
n <- 4  # sample size
X <- rt(n, df = 1) + mu.star

## Function calculates  the log-likelihood
log.like <- function(mu, X)
{
  n <- length(X)
  rtn <- -n*log(pi) - sum( log(1 + (X - mu)^2) )
  return(rtn)
}

mu.x <- seq(-10, 40, length = 1e3)  # A sequence of mu's 
ll.est <- sapply(mu.x, log.like, X)  # evaluating log-likelihood at the mus
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))  # plotting log-likelihood. Not concave, so we need to choose good starting values.


## Starting Newton-Raphson method
tol <- 1e-5  # tolerance level for when to stop algorithm


## Returns derivate of log-likelihood
f.prime <- function(X, mu)
{
  rtn <- sum(2* (X - mu)/(1 + (X-mu)^2))  #f.prime
  return(rtn)
}

## Loop below stops when |mu_(k+1) - mu_(k)| < tol

current <- -10 # Good starting value
diff <- 100  # inital large value for difference
iter <- 0    # counting the number of iterations

mu.k <- current
while( (diff > tol) && iter < 1000)
{
  iter <- iter + 1
  update <- current - f.prime(X, current)/(-2*n)  # NR update
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(mu.k, evals, pch = 16, col = rgb(0,0,1, alpha = .5))
abline(v = current, lty = 2)



### Lab 9 Class Demonstration


################################################
## Old Faithful Geyser data
################################################
data(faithful)
head(faithful)

x <- faithful$eruptions
hist(x, breaks = 30, main = "Eruptions")


################################################
## EM Algorithm for the Old Faithful Geyser data
################################################
## Estep gamma calculation
## this is a general function that we are not actuall
gamma_ick <- function(x, mu, sig2, pis, C = 2)
{
  gamma_prob <- matrix(0, nrow = length(x), ncol = C)
  for(c in 1:C)
  {
    gamma_prob[ ,c]  <- dnorm(x, mean = mu[c], sd = sqrt(sig2[c]))* pis[c]
  }
  
  gamma_prob <- gamma_prob/(rowSums(gamma_prob))
  return(gamma_prob)
}


# Starting values
pis <- c(.6, .4) 
mu <- c(1, 5)
sig2 <- c(1, 2)
diff <- 100
tol <- 1e-5
iter <- 0

# just for visualizing
current <- c(pis, mu, sig2)
store <- current
C <- 2
while(diff > tol)
{
  previous <- current
  iter <- iter + 1
  
  # E step: find gamma_{i,c,k} for just c = 1, since for c = 2 is just 1-Ep
  # Ep <- current[1]*dnorm(x, current[2], sqrt(current[4]))/
  #   (current[1]*dnorm(x, current[2], sqrt(current[4])) + (1 - current[1])*dnorm(x, current[3], sqrt(current[5])))
  # 
  Ep <- gamma_ick(x, mu, sig2, pis, C = 2)
  
  # M-step
  pis <- colMeans(Ep)
  mu <- colSums(Ep*x) / colSums(Ep)
  for(c in 1:C)
  {
    sig2[c] <- sum(Ep[,c]*(x - mu[c])^2) / sum(Ep[,c])
  }
  current <- c(pis, mu, sig2)
  
  diff <- norm(previous - current, "2")
  store <- rbind(store, current)
}

current # final estimates


# Final estimates of the probability
# that each observation is in Class C.
Prob.Z <- current[1]*dnorm(x, current[2], sqrt(current[4]))/
  (current[1]*dnorm(x, current[2], sqrt(current[4])) + (1 - current[1])*dnorm(x, current[3], sqrt(current[5])))

head(round(Prob.Z, 10))


# Make plot of iterative model fits
hist(x, breaks = 30, main = "Eruptions", freq = FALSE)
for(i in 1:dim(store)[1])
{
  test.x <- seq(min(x), max(x), length = 1000)
  test.y <- store[i,1]* dnorm(test.x, mean = store[i,3], sd = sqrt(store[i,5])) + (store[i,2]) *dnorm(test.x, mean = store[i,4], sd = sqrt(store[i,6]))
  lines(test.x, test.y, col = rgb(1,0,0, alpha = .5))
  i <- i + 1
}
lines(test.x, test.y, col = rgb(0,0,1, alpha = 1))

# add color
color <- 1*(Ep < .5) + 3*(Ep >= .5)
points(x, rep(0, length(x)), pch = 16, col = color)



### Lab 12 Class Demonstration


#########################################
## Visualizging SGA objective functions
#########################################
# consider N(mu, 1) and the likelihood for mu
set.seed(1)
mu <- 5
n <- 1e3
dat <- rnorm(n, mean = mu, sd = 1)
hist(dat)

# plotting objective functions
logf <- function(x, mu)
{
  -sum((x-mu)^2/2)/length(x)
}

# finding gradient
f.gradient <- function(x, mu)
{
  -sum(x - mu)/length(x)
}

######### full loglikelihood #########
B <- 5e2
mu.ax <- seq(-5, 15, length = B)
log.like <- length(B)
for(i in 1:length(mu.ax))
{
  log.like[i]  <- logf(dat, mu.ax[i])
}
plot(mu.ax, log.like, type = 'l', lwd = 2.5)


######### SGA without batching #########
sga.like <- matrix(0, nrow = B, ncol = 1e2)
for(j in 1:1e2)
{
  chosen <- sample(1:n, 1) # stochastic part
  for(i in 1:length(mu.ax))
  {
    sga.like[i,j]  <- logf(dat[chosen], mu.ax[i])
  }
  lines(mu.ax, sga.like[,j], col =  adjustcolor("red", alpha.f = .4))
  points(y = 1, x = dat[chosen], pch = 16, col = "purple")
  #Sys.sleep(2)
}




######### Mini Batch #########
sga.like <- matrix(0, nrow = B, ncol = 1e2)
# likelihood for each randomly chosen datapoint
for(j in 1:1e2)
{
  chosen <- sample(1:n, 100) #mini batch of size 10
  for(i in 1:length(mu.ax))
  {
    sga.like[i,j]  <- logf(dat[chosen], mu.ax[i])
  }
  lines(mu.ax, sga.like[,j], col =  adjustcolor("blue", alpha.f = .4))
  points(y = 1, x = mean(dat[chosen]), pch = 16, col = "orange")
  #Sys.sleep(.2)
}


#########################################
## Now adding SGA optimization steps 
## for better visualizations
#########################################

######### full loglikelihood #########
B <- 5e2
mu.ax <- seq(-5, 15, length = B)
log.like <- length(B)
for(i in 1:length(mu.ax))
{
  log.like[i]  <- logf(dat, mu.ax[i])
}
plot(mu.ax, log.like, type = 'l', lwd = 2.5)

sga.like <- matrix(0, nrow = B, ncol = 1e2)

######### SGA without batching #########
for(j in 1:1e2)
{
  chosen <- sample(1:n, 1)
  for(i in 1:length(mu.ax))
  {
    sga.like[i,j]  <- logf(dat[chosen], mu.ax[i])
  }
  lines(mu.ax, sga.like[,j], col =  adjustcolor("red", alpha.f = .2))
  #Sys.sleep(.2)
}


max.iter <- 1e3
mu_k <- -4
t <- .2
mu.store <- numeric(length = max.iter)
for(k in 1:max.iter)
{
  old <- mu_k
  chosen <- sample(1:n, 1)
  mu_k <- old - t*f.gradient(dat[chosen], old)
  mu.store[k] <- mu_k
}

for(k in 1:100)
{
  points(x = mu.store[k], y = -45, col = adjustcolor("blue", alpha.f = .2), pch = 16)
  #Sys.sleep((max.iter - k)/max.iter)
}
#hist(mu.store, add = TRUE)
abline(v = mean(mu.store), col = "blue", lty = 2)



#########################################
## Now adding SGA optimization steps 
##  for mini-batch
#########################################

######### full loglikelihood #########
B <- 5e2
mu.ax <- seq(-5, 15, length = B)
log.like <- length(B)
for(i in 1:length(mu.ax))
{
  log.like[i]  <- logf(dat, mu.ax[i])
}
plot(mu.ax, log.like, type = 'l', lwd = 2.5)
sga.like <- matrix(0, nrow = B, ncol = 1e2)

######### SGA with mini-batching size 32 #########
for(j in 1:1e2)
{
  chosen <- sample(1:n, 10)
  for(i in 1:length(mu.ax))
  {
    sga.like[i,j]  <- logf(dat[chosen], mu.ax[i])
  }
  lines(mu.ax, sga.like[,j], col =  adjustcolor("red", alpha.f = .2))
  #Sys.sleep(.2)
}
#abline(v = c(min(dat), max(dat)), col = "blue", lty = 2)

max.iter <- 1e3
mu_k <- -4
t <- 1
mu.store <- numeric(length = max.iter)
for(k in 1:max.iter)
{
  old <- mu_k
  chosen <- sample(1:n, 32)
  mu_k <- old - t*f.gradient(dat[chosen], old)
  mu.store[k] <- mu_k
}

# viewing first 100 points
for(k in 1:100)
{
  points(x = mu.store[k], y = -45, col = adjustcolor("blue", alpha.f = .2), pch = 16)
  #Sys.sleep((max.iter - k)/max.iter)
}
abline(v = mean(mu.store), col = "blue", lty = 2)


#### Class 12 Class Demonstration Part b


#####################################
## Simulated Annealing
## Demonstrative example
#####################################
set.seed(1)
fn <- function(x, T = 1)
{
  h <- ( cos(50*x) + sin(20*x) )^2 
  exp(h/T) * (0 < x & x < 1)
}

x <- seq(0, 1, length = 5e2)
plot(x, fn(x), type = 'l', ylim = c(0,150), ylab = "exp(f/T)")
for(t in 2:4)
{
  lines(x, fn(x, T = 1/(log(t))), col = (t))
}
legend("topright", col = 1:4, lty = 1, legend = c("T = 1", "T = .83", "T = .75", "T = .71"))


simAn <- function(N = 10, r = .3)
{
  x <- numeric(length = N)
  x[1] <- runif(1)
  
  for(k in 2:N)
  {
    a <- runif(1, x[k-1] - r,  x[k-1] + r)
    T <- 1/(log(k))
    
    ratio <- fn(a,T)/fn(x[k-1], T)  #fn is the exp(h/T)
    if( runif(1) < ratio)
    {
      x[k] <- a 
    } else{
      x[k] <- x[k-1] 
    }
  }
  return(x) 
}

N <- 500
sim <- simAn(N = N)
sim[which.max(fn(sim))]  # theta^*

plot(x, fn(x), type = 'l', ylab = "exp(f/T)")
points(sim, fn(sim), pch = 16, col = adjustcolor("blue", alpha.f = .4))


#####################################
## Location Cauchy likelihood example
#####################################

################################################
## MLE for location Cauchy distribution using
## Newton-Raphson method
## We will plot the likelihood as well
################################################
set.seed(1)
mu.star <- 5  # Setting true mu
n <- 4  # sample size
X <- rt(n, df = 1) + mu.star

## Function calculates the exp(like/T)
log.like <- function(mu, X, T = 1)
{
  n <- length(X)
  rtn <- -n*log(pi) - sum( log(1 + (X - mu)^2) )
  return(exp(rtn/T))
}

mu.x <- seq(-10, 40, length = 1e3)  # A sequence of mu's 
ll.est <- log(sapply(mu.x, log.like, X))  # evaluating log-likelihood at the mus
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))  # plotting log-likelihood. Not concave, so we need to choose good starting values.

# Simulated annealing algorithm
simAn <- function(N = 10, r = .5)
{
  x <- numeric(length = N)
  x[1] <- runif(1, min = -10, max = 40)
  fn.value <- numeric(length = N)
  
  fn.value[1] <- log.like(mu = x[1], X, T = 1)
  for(k in 2:N)
  {
    a <- runif(1, x[k-1] - r, x[k-1] + r)
    T <- 1/(1 + log(log(k)))
    ratio <- log.like(mu = a, X, T)/log.like(mu = x[k-1], X, T)
    if( runif(1) < ratio)
    {
      x[k] <- a
    } else{
      x[k] <- x[k-1]
    }
    fn.value[k] <- log.like(mu = x[k], X, T = 1)
  }
  x
  return(list("x" = x, "fn.value" = fn.value)) 
}

par(mfrow = c(2,2))

# Four different runs all converge.
sim <- simAn(N = 1e2, r = 5)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu)) 
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("blue", alpha = .4))

sim <- simAn(N = 1e2, r = 5)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("darkred", alpha = .4))

sim <- simAn(N = 1e2, r = 5)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("darkgreen", alpha = .4))

sim <- simAn(N = 1e2, r = 5)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("purple", alpha = .4))


par(mfrow = c(1,2))
## Different values of r
# very large r
sim <- simAn(N = 1e3, r = 500)
plot(mu.x, ll.est, type = 'l', main = "r = 500. Many rejections",  ylab = "log-likelihood", xlab = expression(mu))  
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("blue", alpha = .2))

#very small r
plot(mu.x, ll.est, type = 'l', main = "r = .1. Many small acceptances", ylab = "log-likelihood", xlab = expression(mu)) 
sim <- simAn(N = 1e3, r = .1)
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("blue", alpha = .2))

plot.ts(store.grad)