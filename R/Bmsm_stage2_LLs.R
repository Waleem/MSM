#' Internal Routine Used By \code{\link{Bmsm}} Model.
#'
#' Internal routine used by \code{\link{Bmsm}} model to calculate standard errors.
#'
#' @param para is a vector of stage 1 \code{\link{Bmsm}} parameters.
#' @param kbar is the number of frequency components in the \code{\link{Bmsm}}(k) model.
#' @param dat is a matrix of returns.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' If data is monthly, then set n.vol to 12. If data is quarterly, then n.vol should be 4.
#' @param para1 is a vector of first stage parameters [m01, m02, sigma1, sigma2, gamma_k, b].
#'
#' @return Log-likelihood values.
#'
Bmsm_stage2_LLs <- function(para, kbar, dat, n.vol, para1){

  para <- Bmsm_check_lls2_para(para)

  m01     <- para1[1]
  m02     <- para1[2]
  sigma1  <- para1[3]/sqrt(n.vol)
  sigma2  <- para1[4]/sqrt(n.vol)
  gamma.k <- para1[5]
  b       <- para1[6]
  rhoe    <- para[1]
  lamda   <- para[2]
  rho.m   <- para[3]

  k2 <- 4^kbar

  gm <- Bmsm_states(m01, m02, kbar)
  A  <- Bmsm_A(kbar, b, gamma.k, lamda, rho.m)
  N <- nrow(dat);

  LLs <- Bmsm_filtered_cpp(dat, A, gm, rhoe, sigma1, sigma2)$LLs


  return(LLs)
}
