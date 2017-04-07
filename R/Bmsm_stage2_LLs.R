Bmsm_stage2_LLs <- function(para, kbar, dat, n.vol, para1){
  
  para <- Bmsm_check_lls2_para(para)
  
  m01     <- para1[1]
  m02     <- para1[2]
  sigma1  <- para1[3]/sqrt(n.vol)
  sigma2  <- para1[4]/sqrt(n.vol)
  gamma.k <- para1[5]
  b       <- para1[6]
  rhoe    <- para[1]
  lamda   <- para[2]
  rho.m   <- para[3]
  
  k2 <- 4^kbar
  
  gm <- Bmsm_states(m01, m02, kbar)
  A  <- Bmsm_A(kbar, b, gamma.k, lamda, rho.m)
  N <- nrow(dat); 
  
  LLs <- Bmsm_filtered_cpp(dat, A, gm, rhoe, sigma1, sigma2)$LLs
  

  return(LLs)
}
