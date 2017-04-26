#' Combined Univariate Log-Likelihood  For \code{\link{Bmsm}}(k) model.
#'
#' Calculates sum of the combined univariate log-likelihood values for \code{\link{Bmsm}}(k) model.
#' This is the first stage log-likelihood estimation for a \code{\link{Bmsm}} model.
#' This stage estimates the first six parameters [m01, m02, sigma1, sigma2, gamma_k, b].
#'
#' @param para is a 6 elements vector of parameters [m01, m02, sigma1, sigma2, gamma_k, b].
#' @param kbar is the number of frequency components in the \code{\link{Bmsm}}(k) model.
#' @param dat is a matrix of returns.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' If data is monthly, then set n.vol to 12. If data is quarterly, then n.vol should be 4.
#'
#' @return sum of the combined log-likelihood: L1 + L2
#'
#'
#' @examples
#' data("calvet2006returns")
#' ret <- as.matrix(calvet2006returns[,2:3])*100
#' L <- Bmsm_stage1_likelihood(c(1.4330, 1.5768, 5.0433, 7.6284, 0.7299, 8.2716), ret, 2, 252)
#'
#' @export
Bmsm_stage1_likelihood <- function(para, dat, kbar, n.vol){

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

  ll.1 <- Msm_ll2(par1, kbar, ret1, n.vol)
  ll.2 <- Msm_ll2(par2, kbar, ret2, n.vol)

  ll <- ll.1 + ll.2

  return(ll)
}
