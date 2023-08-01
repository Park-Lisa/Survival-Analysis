#MCMC for Bayesian Poisson regression
mcmc <- function(x, y, n.sim, init, step) { p <- ncol(x)
n <- nrow(x)
post.beta <- matrix(0, n.sim, p)
ac.ratio <- rep(0, n.sim) prior.m <- 0
prior.s <- 1000^3
ifelse(is.null(init), init <- rep(0, p), init <- init)
post.beta[1, ] <- beta <- init eta <- x%*%beta
mu <- exp(eta)
log.prior <- sum(dnorm(beta, prior.m, prior.s, log = T)) log.like <- sum(y*log(mu) - mu)
for (i in 2:n.sim) {
  beta.new <- beta + rnorm(p, 0, step) eta.new <- x%*%beta.new
  mu.new <- exp(eta.new)
  log.prior.new <- sum(dnorm(beta.new, prior.m, prior.s, log= T)) log.like.new <- sum(y*log(mu.new) - mu.new)
  temp <- exp((log.prior.new + log.like.new) - (log.like + log.prior)) rho <- min(1, temp)
  if (runif(1) < rho) { ac.ratio[i] <- 1 beta <- beta.new eta <- x%*%beta
  mu <- exp(eta)
  log.prior <- log.prior.new
  log.like <- log.like.new
  }
  post.beta[i,] <- beta
}
obj <- list(posterior = post.beta, acpt.ratio = mean(ac.ratio))
return(obj) }