# soft-thresholding operator
S <- function(z, lambda) 
{
  (z - lambda) * (z > lambda) + 
    (z + lambda) * (z < -lambda) + 
    0 * (abs(z) <= lambda)
}

# LASSO update function 
lasso.update <- S

# coordinate decent algorithm
ncv <- function(x, y, lambda, a = 3.7, init = rep(0, p), max.iter = 100, eps = 1.0e-8)
{
  n <- length(y)
  p <- ncol(x)
  
  # marginal standardization of x
  x <- scale(x)
  m <- attr(x, "scaled:center")
  s <- attr(x, "scaled:scale")
  
  # centering of y
  my <- mean(y)
  y <- (y - my)
  
  # initialize beta
  beta <- init
  
  # residual
  r <- (y - x %*% beta)
  
  update.ft <- function(x, lambda) lasso.update(x, lambda)
 
  # start update
  for (t in 1:max.iter)
  {
    new.beta <- beta
    for (j in 1:p)
    {
      xj <- 1/n * crossprod(x[,j],  r) + beta[j]
      new.beta[j] <- update.ft(xj, lambda/s[j]) 
      r <- r - (new.beta[j] - beta[j]) * x[,j]  
    }
    if (max(abs(beta - new.beta)) < eps) break
    beta <- new.beta
  }
  
  # transform back
  beta <- beta / s[j]
  beta0 <- my - m %*% beta
  
  index <- which(abs(beta) > eps)
  beta.info <- beta[index]
  
  obj = list(intercept = beta0,
             beta = beta.info,
             index = index)
}

