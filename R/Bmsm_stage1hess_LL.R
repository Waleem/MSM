#' Internal Routine Used By \code{\link{Bmsm}} Model.
#'
#' Internal routine used by \code{\link{Bmsm}} model to calculate standard errors.
#'
#' @param para is a vector of stage 1 \code{\link{Bmsm}} parameters.
#' @param kbar is the number of frequency components in the \code{\link{Bmsm}}(k) model.
#' @param dat is a matrix of returns.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' If data is monthly, then set n.vol to 12. If data is quarterly, then n.vol should be 4.
#'
#' @return Log-likelihood values.
#'
Bmsm_stage1hess_LL <- function(para, kbar, dat, n.vol){

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

  LL <- Msm_likelihood2(par1, kbar, ret1, n.vol)$LL + Msm_likelihood2(par2, kbar, ret2, n.vol)$LL

  return(LL)
}
