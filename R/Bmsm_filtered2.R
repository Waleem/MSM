#' Filtered Probability for \code{\link{Bmsm}}(k) model.
#'
#' Calculates filtered probability and log-likelihood values for \code{\link{Bmsm}}(k) model.
#'
#' @param para is a vector of parameters returned by \code{\link{Bmsm}}.
#' @param kbar is the number of frequency components in the \code{\link{Bmsm}}(k) model.
#' @param dat is a matrix of returns.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#'
#' @return a list consisting of:
#' \item{LL}{sum of loglikelihood values at optimum}
#' \item{LLs}{a vector of loglikelihood values at optimum}
#' \item{filtered}{a matrix of filtered probabilities}
#' \item{A}{the estimated transition matrix}
#' \item{g.m}{the estimated state values}
#'
#' @examples
#' data("calvet2006returns")
#' ret <- as.matrix(calvet2006returns[,2:3])*100
#' fit <- Bmsm(ret, kbar=2, n=252, s.err=FALSE)
#' x   <- Bmsm_filtered2(fit$para, 2, ret, 252)
#'
#' @export
Bmsm_filtered2 <- function(para, kbar, dat, n.vol){


  m01     <- para[1]
  m02     <- para[2]
  sigma1  <- para[3]/sqrt(n.vol)
  sigma2  <- para[4]/sqrt(n.vol)
  gamma.k <- para[5]
  b       <- para[6]
  rhoe    <- para[7]
  lamda   <- para[8]
  rho.m   <- para[9]

  k2 <- 4^kbar

  gm <- Bmsm_states(m01, m02, kbar)
  A  <- Bmsm_A(kbar, b, gamma.k, lamda, rho.m)
  N <- nrow(dat);

  likelihood <- Bmsm_filtered_cpp(dat, A, gm, rhoe, sigma1, sigma2)



  likelihood$A    <- A
  likelihood$g.m  <- gm
  likelihood$para <- para
  likelihood$kbar <- kbar
  likelihood$n    <- n.vol

  return(likelihood)
}
