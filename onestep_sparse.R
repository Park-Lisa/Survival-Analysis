# One-step Sparse Estimation


one_estim <- function(X, y, a = 3.7, lambda, max.iter = 1000, init = NULL, eps = 1.0e-8) { 
n <- length(y)
p <- ncol(x)

# marginal standardization of x
x <- scale(x)
m <- attr(x, "scaled:center") s <- attr(x, "scaled:scale")
# centering of y
my <- mean(y) y <- (y - my)
# initialize beta
beta <- coef(lm(y ~ x - 1))
r <- (y - x %*% beta)
new.beta <- beta 
for (j in 1:p) {
  xj <- 1/n * crossprod(x[,j], r) + beta[j] wj <- dp(abs(beta[j]), lambda / s[j], a) new.beta[j] <- Soft(xj, wj)
  r <- r - (new.beta[j] - beta[j]) * x[,j]
}
beta <- new.beta
# transform back
beta <- beta / s
beta0 <- my - m %*% beta
index <- which(abs(beta) > eps) beta.info <- beta[index]
obj = list(beta = c(intercept = beta0, beta.info),
           index = index)
}