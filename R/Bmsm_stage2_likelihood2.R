#' Log-Likelihood  For \code{\link{Bmsm}}(k) model.
#'
#' Calculates sum of log-likelihood values for \code{\link{Bmsm}}(k) model.
#' This is the second stage log-likelihood estimation for a \code{\link{Bmsm}} model.
#' This stage estimates the last three parameters [rhoe, lamda, rho.m].
#'
#' @param para is a 3 elements vector of parameters [rhoe, lamda, rho.m].
#' @param kbar is the number of frequency components in the \code{\link{Bmsm}}(k) model.
#' @param dat is a matrix of returns.
#' @param para1 is a vector of first stage parameters [m01, m02, sigma1, sigma2, gamma_k, b].
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' If data is monthly, then set n.vol to 12. If data is quarterly, then n.vol should be 4.
#'
#' @return sum of log-likelihood: L
#'
#'
#' @examples
#' data("calvet2006returns")
#' ret <- as.matrix(calvet2006returns[,2:3])*100
#' para1 <- c(1.4330, 1.5768, 5.0433, 7.6284, 0.7299, 8.2716)
#' para <- c(0.1090, 0.4421, 0.8823)
#' L <- Bmsm_stage2_likelihood2(para, 2, ret, para1, 252)
#'
#' @export
Bmsm_stage2_likelihood2 <- function(para, kbar, dat, para1, n.vol){

  rhoe  <- para[1]
  lamda <- para[2]
  rho.m <- para[3]

  k2 <- 4^kbar

  m01     <- para1[1]
  m02     <- para1[2]
  sigma1  <- para1[3]/sqrt(n.vol)
  sigma2  <- para1[4]/sqrt(n.vol)
  gamma.k <- para1[5]
  b       <- para1[6]

  gm <- Bmsm_states(m01, m02, kbar)
  A  <- Bmsm_A(kbar, b, gamma.k, lamda, rho.m)
  pa <- 1/(2*pi*sqrt(1-rhoe^2))
  N <- nrow(dat);

  LL <- Bmsm_stage2_ll_cpp(dat, A, gm, rhoe, sigma1, sigma2)

  if(!is.finite(LL)){

    warning('Log-likelihood is inf. Probably due to all zeros in conditional probability.')
  }

  return(LL)
}
