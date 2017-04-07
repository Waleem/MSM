Bmsm_marginals <- function(smoothed.p,m01,m02,kbar){
  

  Malpha <- Msm_clustermat(m01,kbar)
  Mbeta  <- Msm_clustermat(m02,kbar)
  M      <- Bmsm_clustermat(m01,m02,kbar)
  
  M1 <- M[,1:kbar]
  M2 <- M[,(kbar+1):ncol(M)]
  
  palpha <- matrix(0,nrow(smoothed.p),2^kbar)
  pbeta  <- matrix(0,nrow(smoothed.p),2^kbar)
  
  for (i in 1:(2^kbar)) {
    
    col.sel <- apply(M1 == (matrix(rep(Malpha[i,],4^kbar),nrow=4^kbar,byrow=T)), 1, all)
    palpha[,i]  <- apply(matrix(smoothed.p[,col.sel],ncol=sum(col.sel==TRUE)),1,sum)
    
    col.sel <- apply(M2 == (matrix(rep(Mbeta[i,],4^kbar),nrow=4^kbar,byrow=T)), 1, all)
    pbeta[,i]   <- apply(matrix(smoothed.p[,col.sel],ncol=sum(col.sel==TRUE)),1,sum)
    
  }
  
  marginals <- list(
    palpha = palpha,
    pbeta  = pbeta,
    Malpha = Malpha,
    Mbeta  = Mbeta
  )
  return(marginals)
  
}