Bmsm_stage1_likelihood <- function(para, dat, kbar, n.vol){

  m01     <- para[1]
  m02     <- para[2]
  sigma1  <- para[3]
  sigma2  <- para[4]
  gamma.k <- para[5]
  b       <- para[6]

  ret1 <- matrix(dat[,1],ncol=1)
  ret2 <- matrix(dat[,2],ncol=1)
  par1 <- c(m01,b,gamma.k, sigma1)
  par2 <- c(m02,b,gamma.k, sigma2)
  
  ll.1 <- Msm_ll2(par1, kbar, ret1, n.vol)
  ll.2 <- Msm_ll2(par2, kbar, ret2, n.vol)
  
  ll <- ll.1 + ll.2

  return(ll)
}
