

#### Quiz 1 

### Question 1

A <- matrix(c(1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 
              1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 ,
              0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 ,
              0 , 0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 1),
            nrow = 7, ncol = 12, byrow = TRUE)

f.con <- rbind(A, diag(12))
f.rhs <- c(1000, 1500, 1200, 700, 900, 1200, 900, rep(0,12))

f.obj <- c(65,57,22,42,36,30,30,60,65,70,55,42)
f.dir <- c("=", "=", "=", "=", "=", "=", "=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=")

library(lpSolve)
Sol <- lp ("min", f.obj, f.con, f.dir, f.rhs)
Sol$objval
Sol$solution

greeks=c(alpha='\u03b1', beta='\u03b2', gamma='\u03b3')
data = matrix(Sol$solution, ncol = 4, byrow=TRUE)
colnames(data) = c('A', 'B', 'C', 'D')
rownames(data) = paste0(c(greeks['alpha'], greeks['beta'], greeks['gamma']))
final = as.table(data)
final

### Question 2

print(c("1-2", "1-3", "1-4", "2-3", "2-4", "2-6", "2-7", "3-4", "3-5", "4-6"))
print(c("1-3")) # share neighbor nodes 2, 4, and 6.

A = matrix(c(0, 1, 1, 1, 0, 1, 0, 0,
             1, 0, 1, 1, 1, 0, 0, 0,
             1, 1, 0, 1, 0, 1, 1, 0,
             1, 1, 1, 0, 0, 0, 0, 0,
             0, 1, 0, 0, 0, 0, 1, 0,
             1, 0, 1, 0, 0, 0, 0, 1,
             0, 0, 1, 0, 1, 0, 0, 0,
             0, 0, 0, 0, 0, 1, 0, 0), nrow = 8, ncol = 8, byrow = T)

Deg = diag(c(4,4,5,3,2,3,2,1))
L = Deg - A

LL <- L
LL[1,] <- LL[1,]/LL[1,1]
LL[2,] <- LL[2,]/LL[2,2]
LL[3,] <- LL[3,]/LL[3,3]
LL[4,] <- LL[4,]/LL[4,4]
LL[5,] <- LL[5,]/LL[5,5]
LL[6,] <- LL[6,]/LL[6,6]
LL[7,] <- LL[7,]/LL[7,7]
LL[8,] <- LL[8,]/LL[8,8]

E <- eigen(LL)
#E$values
EE = sum(E$values)

ifelse(EE == 8, TRUE, FALSE)



####  Quiz 2

set.seed(1)

### Problem 1###

n_seq <- c(1e1, 5e1, 1e2, 5e2, 1e3, 5e3)  ### sequence of sample size
N <- max(n_seq) 
alpha <- 2
beta <- 5

### draw a sample of maximum size from the Weibull distribution
y <- rweibull(N, shape = alpha, scale = beta)

##### log-likelihood function for Weibull
weib_like <-  function(a, y){
  n <- length(y)
  loglike <- n*log(alpha)+(alpha-1)* sum(log(y))-n*alpha*log(a)-sum((y/a)^(alpha))
  return(-loglike)
}

bet_mle_opt <- vector(length = length(n_seq))  ### beta mle using optim 
bet_mle_th <- vector(length = length(n_seq))   ### beta mle using theory
bet_seq <- seq(0.01, 20, length = 1e4) # a sequence of beta's
ll_eva <- matrix(nrow = length(bet_seq), ncol = length(n_seq)) ##log-likelihood eval for bet_seq  

#finding mle using optim and theory 
for (k in 1:length(n_seq)){
  s <- n_seq[k]
  bet_mle_opt[k] <- optim(beta, weib_like, y = y[1:s])$par
  bet_mle_th[k] <- (mean((y[1:s])^(alpha)))^(1/alpha)
  ll_eva[ ,k] <- sapply(bet_seq, weib_like, y[1:s])
}
bet_mle_th
bet_mle_opt

######Problem 2 ###
#### function to see if new observation lies in the confidence interval or not
est_prob <- function(theta, n, alpha){
  y <- rnorm(n, mean = theta, sd = 1)
  upper_ci <- mean(y) + (1/sqrt(n))*qnorm(1-alpha/2)
  lower_ci <- mean(y) - (1/sqrt(n))*qnorm(1-alpha/2)
  y101 <- rnorm(1, mean = theta, sd = 1)
  coverage <- ifelse(y101 > lower_ci & y101 < upper_ci, 1, 0)
  return(coverage)
}

iter_seq <- c(1e1, 1e2, 1e3, 1e4, 1e5) ### sequence of replications
iter_num <- max(iter_seq) ### number of maximum replications
store_cov <- vector(length = length(iter_num))

for (i in 1:iter_num) {
  store_cov[i] <- est_prob(10, 100, 0.05)
}

p_est <- vector(length = length(iter_seq))
p_sd <- vector(length = length(iter_seq))

for (i in 1:length(iter_seq)) {
  s <- iter_seq[i]
  p_est[i] <- mean(store_cov[1:s])
  p_sd[i] <- sd(store_cov[1:s])/sqrt(s)
}
p_est   # estimate of p for different replications
p_sd    # se of p for different replications
CI.up = p_est + p_sd
CI.dn = p_est - p_sd

###Plot of estimated p versus replication##
plot(p_est ~ log(iter_seq), xlab = "number of replictaions", ylab = "p_est", type = "l", ylim=c(0,0.3), col='red')
arrows(log(iter_seq), CI.dn, log(iter_seq), CI.up, code=3, length=0.2, angle=90, col='blue')


#### Quiz 3

set.seed(1)

################################# Question 1

samp <- 50
unif <- runif(samp, 0, 1)
X <- numeric(length = length(unif))
for (i in 1:samp)          #### generating samples
{
  ifelse(unif[i] < 0.333, X[i] <- rnorm(1, 0, 2), X[i] <- rnorm(1, 0, 4))
}

eps <- rnorm(samp)        #### generating errors
beta0 <- 7.5       # true intercept
beta1 <- 0.68      # true slope

Y <- beta0 + beta1*X + eps   # population regression function
lnr.fit <- lm(Y ~ X)      # fitting the model
summary(lnr.fit)
beta0.hat <- as.numeric(coef(lnr.fit)[1])
beta1.hat <- as.numeric(coef(lnr.fit)[2])
rand <- seq(1, 100)
plot(rand, beta0 + beta1*rand, type = "l", col = "blue")
lines(rand, beta0.hat + beta1.hat*rand , type = "l", col = "red")
legend("topleft", c("truth", "est"), col = c("blue", "red"), cex = 1, lty = 1, lwd = 2)

n <- c(100, 200, 500, 1000, 5000, 10000)
n.max <- max(n)
unif <- runif(n.max, 0, 1)
X <- numeric(length = length(unif))
for (i in 1:n.max)          #### generating samples
{
  ifelse(unif[i] < 0.333, X[i] <- rnorm(1, 0, 2), X[i] <- rnorm(1, 0, 4))
}
eps <- rnorm(n.max)
Y <- beta0 + beta1*X + eps

beta0_hat <- numeric(length = length(n))     ### vector to save different est. intercepts
beta1_hat <- numeric(length = length(n))     ### vector to save different est. slopes

for (i in 1:length(n))
{
  lnrs.fit <- lm(Y[1:n[i]] ~ X[1:n[i]])
  beta0_hat[i] <- as.numeric(coef(lnrs.fit)[1])
  beta1_hat[i] <- as.numeric(coef(lnrs.fit)[2])
}

x <- beta0/beta0_hat
y <- beta1/beta1_hat
plot(n, x, type = "l", ylim = c(0.95, 1.05), col = "orange", xlab = "sample sizes", ylab = "ratio")
lines(n, y, type = "l", col = "green")
legend("topright", c("x", "y"), col = c("orange", "green"), cex = 1, lty = 1, lwd = 2)
print("Consistent")

################  Question 2

data_quiz <- read.csv("https://www.dropbox.com/s/geke5ykega8lytr/Q3_data.csv?dl=1", header = TRUE)

### Part (1) Fitting and checking significance
lr.fit <- glm(Y ~ X1 + X2 + X3 + X4, data = data_quiz, family = binomial)
summary(lr.fit)
print("X2")

### Part (2) Prediction of probabilities
fit.prob <- predict(lr.fit, type = "response")
fit.pred <- rep(0, nrow(data_quiz))
fit.pred[fit.prob > 0.5] <- 1
mean(fit.pred == data_quiz$Y)


#####  Quiz 4



############### Question 1

m1 <- c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04, 79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
m2 <- c(80.02, 79.74, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97)
t.test(m1, m2)
pval <- as.numeric(t.test(m1, m2)[3])

# at alpha equals 0.1
ifelse(pval < 0.1, paste("Reject null at 0.1 level"), paste("Do NOT reject at 0.1 level"))  
# at alpha equals 0.05
ifelse(pval < 0.05, paste("Reject null at 0.05 level"), paste("Do NOT reject at 0.05 level")) 

#  sample coefficients of variation for the two methods
samp_cv1 <- sd(m1)/mean(m1)
samp_cv2 <- sd(m2)/mean(m2)

# ratio of coefficient of variation
cv_ratio <- samp_cv2/samp_cv1

cat("The likely spread of population 2 is roughly", round(cv_ratio, 0), "times that of population 1")


#########  Question 2

data <- read.table("http://stat4ds.rwth-aachen.de/data/GSS2018.dat", header = TRUE)
x <- data$SEX
y <- data$PRES16

### Part (i)
con_tab <- table(x, y, dnn = c("Sex", "President"))  # contingency table
con_row <- prop.table(con_tab, 1)  # conditional y given x 
con_col <- prop.table(con_tab, 2)  # conditional x given y
con_tab
round(con_row, 3)
round(con_col, 3)

## Part (ii)
chisq.test(x, y)  #  Chi square test
chisq.test(x, y)$expected   # expected frequencies
chisq.test(x, y)$stdres   # standardised residuals
mosaicplot(con_tab, shade = TRUE, main = "Mosaic plot for Votes")


## Quiz 4
########### Question 1 ###########

m1 <- c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04, 79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
m2 <- c(80.02, 79.74, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97)
TT = t.test(m1, m2)
pval <- as.numeric(TT[3])

# at alpha equals 0.1
ifelse(pval < 0.1, paste("Reject null at 0.1 level"), paste("Do NOT reject null at 0.1 level"))  
# at alpha equals 0.05
ifelse(pval < 0.05, paste("Reject null at 0.05 level"), paste("Do NOT reject null at 0.05 level")) 

#  sample coefficients of variation for the two methods
samp_cv1 <- var(m1)/mean(m1)
samp_cv2 <- var(m2)/mean(m2)

# ratio of coefficient of variation
cv_ratio <- samp_cv2/samp_cv1

cat("The likely relative spread of sample 2 is roughly", round(cv_ratio, 0), "times that of sample 1", "\n")

#########  Question 2 ###########

DATA <- read.table("http://stat4ds.rwth-aachen.de/data/GSS2018.dat", header = TRUE)
x <- DATA$PRES16
y <- DATA$SEX

### Part (a)
con_tab <- table(x, y, dnn = c("Sex", "President"))  # contingency table
con_row <- prop.table(con_tab, 1)  # conditional y given x 
con_col <- prop.table(con_tab, 2)  # conditional x given y
con_tab
round(con_row, 3)
round(con_col, 3)

## Part (b)
A = chisq.test(x, y)  #  Chi square test
print(A$expected)   # expected frequencies
print(A$stdres)   # standardised residuals
mosaicplot(con_tab, shade = TRUE, main = "Mosaic plot for votes")

########### Question 3 ###########

DD <- read.table("http://stat4ds.rwth-aachen.de/data/Iris.dat", header=TRUE)
N = dim(DD)[1]

tag1 = which(DD$species == "I.versicolor")
tag2 = which(DD$species == "I.virginica")

Y = rep(0,N)
Y[tag1] = -N/length(tag1)
Y[tag2] = N/length(tag2)

X = cbind(Y,DD[,-c(1,6)])
reg = lm(Y ~ ., data = X)

#XX = cbind(1,DD[,-c(1,6)])
#vec = as.numeric(coef(reg))
XX = DD[,-c(1,6)]
vec = as.numeric(coef(reg)[-1]) #drop the intercept term!
AA = rep(0,N)
for(i in 1:N){
  AA[i] = as.numeric(as.numeric(XX[i,])%*%vec)
}

plot(AA[tag2], type = "o", col = "red", xlab = "Observations", ylab = "Projected
 Value", ylim = c(0,8))
lines(AA[tag1], type = "o", col = "blue")
cat("Complete separation (except one) in the Iris data points in the projected subspace", "\n")