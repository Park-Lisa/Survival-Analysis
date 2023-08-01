#Soft Thresholding Operator
Soft <- function(z, lambda) {
  sign(z) * ifelse((abs(z) > lambda),0)
}

# SCAD update
  scad_update <- function(z, lambda, a = 3.7) { if(abs(z) < 2 * lambda) {
    Soft(z, lambda) }
    else if ((2 * lambda <= abs(z)) & (abs(z) <= a*lambda)) {Soft(z, a*lambda/(a-1)) / (1 - 1/(a-1))}
    else z }
  
  # SCAD penalty
  dp <- function(z, lambda, alpha = 3.7) {
    if(z <= lambda) lambda
    else if((lambda < z) & (z <= alpha*lambda)) (alpha*lambda - z)/(alpha - 1) else 0
  }



lqa <- function(x, y, lambda, a = 3.7, max.iter = 1000, eps = 1.0e-8) { n <- length(y)
p <- ncol(x)

# marginal standardization of x
x <- scale(x)
m <- attr(x, "scaled:center") s <- attr(x, "scaled:scale")

# centering of y
my <- mean(y) y <- (y - my)
# initialize beta
beta <- coef(lm(y ~ x - 1)) r <- (y - x %*% beta)

# update
for (t in 1:max.iter) { new.beta <- beta
for (j in 1:p) {
  xj <- 1/n * crossprod(x[,j], r) + beta[j]
  wj <- dp(abs(beta[j]), lambda / s[j], a) / (2 * abs(beta[j])) new.beta[j] <- xj / (1 + wj)
  r <- r - (new.beta[j] - beta[j]) * x[,j]
  if(abs(new.beta[j]) < 1.0e-3) new.beta[j] <- 0
}
if (max(abs(beta - new.beta)) < eps) 
  break 
beta <- new.beta
}

# transform back
beta <- beta / s
beta0 <- my - m %*% beta
index <- which(abs(beta) > eps) beta.info <- beta[index]
obj = list(beta = c(intercept = beta0, beta.info), index = index) }