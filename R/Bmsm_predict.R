Bmsm_predict <- function(smoothed.p, A, g.m, para, kbar, n.vol, h =NULL){


  m01     <- para[1]
  m02     <- para[2]
  sigma1  <- para[3]/sqrt(n.vol)
  sigma2  <- para[4]/sqrt(n.vol)
  gamma.k <- para[5]
  b       <- para[6]
  rho.e   <- para[7]
  lamda   <- para[8]
  rho.m   <- para[9]

  if (!is.null(h)) smoothed.p <- smoothed.p[nrow(smoothed.p),] %*% A^h

  marginals <- Bmsm_marginals(smoothed.p, m01,m02, kbar)
  #g.m       <- Bmsm_states(m01, m02, kbar)

  palpha <- marginals$palpha
  pbeta  <- marginals$pbeta
  Malpha <- marginals$Malpha
  Mbeta  <- marginals$Mbeta

  rho.t <- rho.e*(smoothed.p %*% matrix(apply(g.m,2,prod),ncol=1)) /
        sqrt( (palpha %*% matrix(apply(Malpha,1,prod), ncol=1)) * (pbeta %*% matrix(apply(Mbeta,1,prod), ncol=1)) )

  cov.t  <- sigma1*sigma2*rho.e * (smoothed.p %*% matrix(apply(g.m,2,prod),ncol=1))
  var1.t <- sigma1^2 * (palpha %*% matrix(apply(Malpha,1,prod), ncol=1))
  var2.t <- sigma2^2 * (pbeta  %*% matrix(apply(Mbeta,1,prod), ncol=1))
  std1.t <- sqrt(var1.t)
  std2.t <- sqrt(var2.t)
  #std1.t <- (sigma1) * (palpha %*% sqrt(matrix(apply(Malpha,1,prod), ncol=1)))
  #std2.t <- (sigma2) * (pbeta  %*% sqrt(matrix(apply(Mbeta,1,prod), ncol=1)))

  covar <- list(
    vol1    = std1.t,
    vol2    = std2.t,
    covt    = cov.t,
    rho.t   = rho.t,
    vol1.sq = var1.t,
    vol2.sq = var2.t
  )

  return(covar)


}
