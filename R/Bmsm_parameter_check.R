Bmsm_parameter_check <- function(dat, kbar, x0, n){
  
  # Check for valid data entry:
  if (!is.matrix(dat)) {
    dat <- as.matrix(dat)
  }
  if (ncol(dat) > 2) {
    dat <- t(dat)
  }
  
  if (ncol(dat) > 2|nrow(dat) < 2|is.null(dat)) stop('data must be a N by 2 numeric matrix.')
  
  # Check for valid parameter inputs:
  if(kbar < 1) stop('kbar (number of volatility components) must be a positive integer.')
  if(n < 1) stop('n must be a positive integer.')
  
  if(!is.null(x0)){
    
    if(length(x0) == 9){
      if (x0[1] < 1 || x0[1] > 1.9999) stop('m01 must be between (1,1.9999]')
      
      if (x0[2] < 1 || x0[2] > 1.9999) stop('m02 must be between (1,1.9999]')
      
      if (x0[3] < 0.00001) stop('sigma1 must be a positive (non-zero) value')
      
      if (x0[4] < 0.00001) stop('sigma2 must be a positive (non-zero) value')
      
      if (x0[5] < 0.0001 || x0[5] > 0.9999) stop('gamma_k must be between [0,1]')
      
      if (x0[6] < 1)  stop('b must be greater than 1')
      
      if (x0[7] < -0.9999 || x0[7] > 0.9999) stop('rho.e must be between [-1,1]')
      
      if (x0[8] < 0.0001  || x0[8] > 0.9999) stop('lamda must be between [0,1]')
      
      if (x0[9] < -0.9999 || x0[9] > 0.9999) stop('rho.m must be between [-1,1]')
      
      
    } else {
      stop('Initial values must be of length 9 in the form c(m01,m02,sigma1,sigma2,gammak,b,rho.e,lamda,rho.m)')
    } 
    
    
  } else {
    x0 <- c(1.5, 1.5, sd(dat[,1]), sd(dat[,2]), .9, 2.5, cor(dat)[2,1], .9, .9)
  }
  
  
  
  lb1 = c(1,   1,   0.00001, 0.00001, 0.001,   1)
  ub1 = c(1.9999,1.9999,100,    100,    0.9999, 100)
  
  lb2 = c(-0.9999,0.0001,-.9999)
  ub2 = c( 0.9999,0.9999,.9999)
  
  bmsm.check <- list(dat        = dat,
                    kbar        = kbar,
                    start.value = x0,
                    lb1         = lb1,
                    ub1         = ub1,
                    lb2         = lb2,
                    ub2         = ub2)   
  return(bmsm.check)
}

