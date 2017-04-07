Bmsm_Transition <- function(gama, lamda, rho.m){
  
  p <- 1-gama+(gama*((1-lamda)*gama+lamda))*((1+rho.m)/4)
  q <- 1-gama+(gama*((1-lamda)*gama+lamda))*((1-rho.m)/4)
  
  T.mat <- c( p, 1-(gama/2)-p, 1-(gama/2)-p, gama-1+p
         ,1-(gama/2)-q, q, gama-1+q, 1-(gama/2)-q
         ,1-(gama/2)-q, gama-1+q, q, 1-(gama/2)-q
         ,gama-1+p, 1-(gama/2)-p, 1-(gama/2)-p, p)
  
  T.mat <- matrix(T.mat,ncol=4, byrow = T)
  
  return(T.mat)
}