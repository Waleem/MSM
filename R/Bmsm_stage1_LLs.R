Bmsm_stage1_LLs <- function(para, kbar, dat, n.vol){
  
  para <- Bmsm_check_lls1_para(para)
  
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
  
  LLs <- Msm_likelihood2(par1, kbar, ret1, n.vol)$LLs + Msm_likelihood2(par2, kbar, ret2, n.vol)$LLs
  
  return(LLs)
}