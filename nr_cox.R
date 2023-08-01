#Newton-Raphson algorithm for Cox’s proportional hazards regression 


nw_cox <- function( Y, X, beta, liklhd, verbose=F, eps=1e-8, iter.max=200 ){
  beta.start <- beta
  lik.start <- lik.old <- liklhd( Y, X, beta.start )
  delta <- solve( lik.old$inform ) %*% lik.old$score
  iter <- 0
  while( (sum( abs(delta) < eps ) < length(beta)) & (iter <= iter.max) ){
    delta <- solve( lik.old$inform ) %*% lik.old$score
    lik.new <- liklhd( Y, X, beta=(beta + delta) )
    if( lik.new$lnlklhd < lik.old$lnlklhd ){
      delta <- delta/2
      beta <- beta - delta
      lik.old <- liklhd( Y, X, beta )
    }
    else{
      beta <- (beta + delta)
      lik.old <- liklhd( Y, X, beta )
      iter <- iter + 1
    }
    if( verbose==T ) cat( "iteration : ", iter, " ; beta = ", round( beta, 7 ), "\n" )
  }
  if( iter > iter.max ) cat( "Convergence not met before maximum iterations reached \n" )
  rslt <- list(  beta0=beta.start, lnlklhd0=lik.start,
                 converge=(iter <= iter.max), nbriter=iter, beta=beta,
                 lnlklhd = lik.old$lnlklhd, score=lik.old$score, inform=lik.old$inform )
  return( rslt )
}