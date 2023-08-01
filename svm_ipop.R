library(kernlab)

# Toy Example
set.seed(2)
n <- 20
x1 <- rnorm(n)
x2 <- rnorm(n)
x <- cbind(x1, x2)
beta <- c(1,1)
e <- rnorm(n, 0, 0)
y <- sign(c(x %*% beta + e))

# plot
plot(x1, x2, type = "n", main = "SVM")
plot(x1, x2, type = "n", main = "SVM")
points(x[y == 1, 1], x[y == 1, 2], col = 2)
points(x[y ==-1, 1], x[y ==-1, 2], col = 4)

# true hyperplane
abline(a = 0, b = -1, lty = 2, col = 1, lwd = 2)

## SVM

# cost parameter
C <- 1

# QP
K <- x %*% t(x)
K.star <- K * (y %*% t(y))

# check ipop function
?ipop

# ipop function arguments
H <- K.star
c <- rep(-1, n)
b <- 0
r <- 0
A <- matrix(y, 1, n)
l <- rep(0, n)
u <- rep(C, n)

# run ipop() function
obj <- ipop(c, H, A, b, l, u, r)

# solution
alpha <- obj@primal 
round(alpha, 3)

# beta
beta <- apply(y * alpha * x, 2, sum)
beta

# support vectors
eps <- 1.0e-7
sv.index <- which(eps < alpha & alpha < (C-eps))
sv.index

# intercept: beta0
temp <- y[sv.index] - x[sv.index,] %*% beta
temp
beta0 <- mean(temp)
beta0

# Plot (estimated beta)
points(x[sv.index,1], x[sv.index,2], pch = 19)

abline(a = -beta0/beta[2], b = -beta[1]/beta[2], col = 3, lwd = 2)
abline(a = (1-beta0)/beta[2], b = -beta[1]/beta[2], col = 3, lty = 3, lwd = 2)
abline(a = (-1-beta0)/beta[2], b = -beta[1]/beta[2], col = 3, lty = 3, lwd = 2)

