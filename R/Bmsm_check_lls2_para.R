Bmsm_check_lls2_para <- function(x){
  
  if (x[1]>=1)  x[1] <- 0.9999
  if (x[1]<=-1) x[1] <- -0.9999
  
  if (x[2]>=1)  x[2] <- 0.9999
  
  if (x[3]>=1)  x[3] <- 0.9999
  if (x[3]<=-1) x[3] <- -0.9999
  
  return(x)
}