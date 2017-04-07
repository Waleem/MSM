Bmsm_A <- function(kbar, b, gamma.kbar, lamda, rho.m){
  

  gamma.k    <- matrix(0,kbar,1)                          
  gamma.k[1] <- 1-(1-gamma.kbar)^(1/(b^(kbar-1)))
  
  
  A <- Bmsm_Transition(gamma.k[1], lamda, rho.m)
  
  if (kbar > 1) {
    
    for (i in 2:kbar) {
      
      gamma.k[i] <- 1-(1-gamma.k[1])^(b^(i-1))
      
      a          <- Bmsm_Transition(gamma.k[i], lamda, rho.m)
      
      # A        <- kronecker(A,a)
      A          <- kronecker(a,A)
    }
      
  }
  
  
  return(A)
}
