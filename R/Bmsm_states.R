Bmsm_states <- function(m01, m02, kbar){
  
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
  
  g1 <- apply(M1,1,prod)
  g2 <- apply(M2,1,prod)
  
  gm <- sqrt(rbind(g1,g2))
  
  return(gm)
  # m1 <- m2 <- list()
  # 
  # for (i in 1:kbar) {
  #   m1[[i]] <- c(m01, 2-m01)
  #   m2[[i]] <- c(m02, 2-m02)
  # }
  # 
  # m <- as.matrix(expand.grid(c(m2,m1)))
  # #  m <- as.matrix(m[,names(m)[seq.int(2^kbar,1)]])
  # 
  # m.col <- ncol(m)/2
  # k2    <- 2^kbar
  # 
  # M2 <- m[,(1:m.col)]
  # M1 <- m[,(m.col+1):k2]
  # 
  # g1 <- apply(M1,1,prod)
  # g2 <- apply(M2,1,prod)
  # 
  # gm <- sqrt(rbind(g1,g2))
  
  
}
