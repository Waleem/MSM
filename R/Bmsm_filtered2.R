Bmsm_filtered2 <- function(para, kbar, dat, n.vol){
  
  
  m01     <- para[1]
  m02     <- para[2]
  sigma1  <- para[3]/sqrt(n.vol)
  sigma2  <- para[4]/sqrt(n.vol)
  gamma.k <- para[5]
  b       <- para[6]
  rhoe    <- para[7]
  lamda   <- para[8]
  rho.m   <- para[9]
  
  k2 <- 4^kbar
  
  gm <- Bmsm_states(m01, m02, kbar)
  A  <- Bmsm_A(kbar, b, gamma.k, lamda, rho.m)
  N <- nrow(dat); 
  
  likelihood <- Bmsm_filtered_cpp(dat, A, gm, rhoe, sigma1, sigma2)



  likelihood$A    <- A
  likelihood$g.m  <- gm
  likelihood$para <- para
  likelihood$kbar <- kbar
  likelihood$n    <- n.vol

  return(likelihood)
}