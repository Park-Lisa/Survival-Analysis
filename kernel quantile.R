library(kernlab)

kqr <- function(x, y, tau, lambda) { 
y <- as.matrix(y)
x <- as.matrix(x)
n <- nrow(X)
H <- ((x %*% t(x)) + diag(rep(1.0e-10, n)))/lambda 
c <- -y
b <- 0; r <- 0
A <- matrix(1, 1, n)
l <- rep(tau - 1, n)
u <- rep(tau, n)
obj <- ipop(c, H, A, b, l, u, r)
theta <- obj@primal
beta_hat <- apply(theta*X, 2, sum) / lambda
eps <- 1.0e-8
beta <- c((1/lambda) * (t(theta) %*% X))
index.theta <- which(abs(theta - tau) > eps & abs(theta - (tau - 1)) > eps) temp <- y[index.theta] - X[index.theta,] %*% beta
beta0 <- mean(temp)
return(c(beta0, beta)) }