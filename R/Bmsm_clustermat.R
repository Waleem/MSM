Bmsm_clustermat <- function(m01, m02, kbar){
  
  m <- list()
  
  for (i in seq(1,2*kbar-1,2)) {
    m[[i]] <- c(m02, 2-m02)
    m[[i+1]] <- c(m01, 2-m01)
  }
  
  m <- as.matrix(expand.grid(m))
  #  m <- as.matrix(m[,names(m)[seq.int(2^kbar,1)]])
  
  m.col <- ncol(m)/2
  k2    <- 2^kbar
  
  M1 <- matrix(m[,seq(2*kbar,1,-2)], nrow=4^kbar) #m[,seq(2*kbar,1,-2)]
  M2 <- matrix(m[,seq(2*kbar-1,1,-2)], nrow=4^kbar) # m[,seq(2*kbar-1,1,-2)]
  
  M <- cbind(M1,M2)

  return(M)

}
