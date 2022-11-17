
for(i in unique(dat$cylinders))
{
  foo <- subset(dat, dat$cylinders == i)
  abline(lm(foo$mpg ~ foo$acceleration), col = i)
}

legend("topleft", col = unique(dat$cylinders), 
       legend = unique(dat$cylinders), pch = 16,
       title = "Cylinders")

for(i in 1:length(levels(iris$Species)))
{
  level <- levels(iris$Species)[i]
  foo <- subset(iris, iris$Species == level)
  abline(lm(foo$Sepal.Width ~ foo$Sepal.Length), col = i)
}

geom_segment(aes(y = 0, x = state, yend = log10(confirmed), xend = state), color = "black")

#cppfunc
cppFunction('NumericVector funcC(NumericVector vec){
double track = 0;
int n = vec.size();
NumericVector ans(n);
for(int i = 0; i< n; i++){
track = track + log(vec[i]);
}

for(int i = 0; i < n; i++){
  ans[i] = log(vec[i])/track;
}
return ans;
}
')



#cpp
sourceCpp("expCountC.cpp")

// [[Rcpp::export]]
NumericVector colSumsC(NumericMatrix x) {
  // Assuming matrices are compatible
  int n = x.nrow();
  int m = x.ncol();
  
  NumericVector out(m);
  
  for(int j = 0; j < m; j++){
    for(int i = 0; i < n; i++){
      out[j] = out[j] + x(i,j);
    }
  }
  return out;
}


#arma
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::export]]
double timesTwo(arma::vec x,arma::vec y) {
  // inner product of two vectors is a matrix
  arma::mat inn(1,1);
  inn = x.t() * y;
  // returning the first element of a 1x1 matrix
  return inn(0);
}



dist[i,j] <- norm(col.mat[i,j, ] - c(0,0,1), "2")
g <- norm(col[1,1, ] - c(0,1,0), "2")
averaging <- rep(0,3)
averaging[1] <- mean(col.mat[ind1, ind2, 1])
ind1 <- (2*(i-1) + 1): (2*i)
reduce[i, j, ] <- averaging
fill = c("grey", adjustcolor("blue", alpha.f = .3))


func2 <- function(vec)
{
  temp <- log(vec)
  return(temp/sum(temp))
}