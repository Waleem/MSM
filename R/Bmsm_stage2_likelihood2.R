Bmsm_stage2_likelihood2 <- function(para, kbar, dat, para1, n.vol){
  
  rhoe  <- para[1]
  lamda <- para[2]
  rho.m <- para[3]
  
  k2 <- 4^kbar
  
  m01     <- para1[1]
  m02     <- para1[2]
  sigma1  <- para1[3]/sqrt(n.vol)
  sigma2  <- para1[4]/sqrt(n.vol)
  gamma.k <- para1[5]
  b       <- para1[6]
  
  gm <- Bmsm_states(m01, m02, kbar)
  A  <- Bmsm_A(kbar, b, gamma.k, lamda, rho.m)
  pa <- 1/(2*pi*sqrt(1-rhoe^2))
  N <- nrow(dat); 
  
  LL <- Bmsm_stage2_ll_cpp(dat, A, gm, rhoe, sigma1, sigma2)

  if(!is.finite(LL)){
    
    warning('Log-likelihood is inf. Probably due to all zeros in conditional probability.')
  }
  
  return(LL)
}