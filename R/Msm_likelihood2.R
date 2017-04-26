#' Log-Likelihood for \code{\link{Msm}}(k) model.
#'
#' Calculates log-likelihood and filtered probability values for \code{\link{Msm}}(k) model.
#'
#' @param para is a vector of parameters [m0, b, gamma_k, sigma].
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#' @param dat is a column matrix of returns.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' If data is monthly, then set n.vol to 12. If data is quarterly, then n.vol should be 4.
#'
#' @return a list consisting of:
#' \item{LL}{sum of loglikelihood values at optimum}
#' \item{LLs}{a vector of loglikelihood values at optimum}
#' \item{filtered}{a matrix of filtered probabilities}
#' \item{A}{the estimated transition matrix}
#' \item{g.m}{the estimated state values}
#'
#'
#' @examples
#' data("calvet2004data")
#' ret <- na.omit(as.matrix(calvet2004data$caret))*100
#' vol <- Msm_likelihood2(c(1.556, 10.92, 0.109, 0.278*sqrt(252)), 2, ret, n.vol=252)
#'
#' @export

Msm_likelihood2 <- function(para, kbar, dat, n.vol){


  m0     <- para[1]
  b      <- para[2]
  gama.k <- para[3]
  sigma  <- para[4]/sqrt(n.vol)
  k2     <- 2^kbar

  A   <- Msm_A(b, gama.k, kbar)
  g.m <- Msm_states(m0, kbar)
  N   <- nrow(dat)

  LLs        <- matrix(0, N, 1)
  pi.mat     <- matrix(0, N+1, k2)
  pi.mat[1,] <-  (1/k2)*matrix(1, 1, k2)


#*----------------------------------------------------------------------*
#*                        Likelihood algorithm                          *
#*----------------------------------------------------------------------*
  sig.mat <-  matrix(rep(sigma*g.m, N), N, ncol(g.m), byrow = N)
  omega.t <-  matrix(dat,N,k2)
  omega.t <-  ((2*pi)^-0.5)*exp( - 0.5*((omega.t/sig.mat)^2))/sig.mat
  omega.t <-  omega.t + 1e-16


  pimat0 = matrix(1/k2, 1, k2)

  likelihood = Msm_likelihood_cpp(pimat0,omega.t,A)

  likelihood$filtered <- likelihood$pmat[-1,]
  likelihood$pmat     <- NULL
  likelihood$A        <- A
  likelihood$g.m      <- g.m

  return(likelihood)

}
