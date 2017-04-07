Bmsm_check_lls1_para <- function(x){
  if (x[1]>=2) x[1] <- 1.9999
  if (x[2]>=2) x[2] <- 1.9999
  if (x[5]>=1) x[5] <- 0.9999
  
  return(x)
}