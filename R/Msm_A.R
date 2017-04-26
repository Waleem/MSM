#' Transition Probability Matrix for \code{\link{Msm}}(k) Model.
#'
#' Calculates transition matrix for \code{\link{Msm}}(k) model. The calculation
#'  exploits the independence of the volatility components. Each \eqn{M_k} has its own
#'  \eqn{2 x 2} transition matrix \eqn{A_k}. It can be shown that the transition matrix
#'  for \eqn{M=[M_1;M_2,..M_{kbar}]} is equal to the kronecker product of all the
#'  transition matrices for all the \eqn{M_k}.
#'
#' @param b is the  growth rate of the switching probabilities of the volatility components and
#' must be in the range (1, inf).
#' @param gamma.kbar is the transition probability of the highest frequency component
#' and must be in the range (0,1).
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#'
#' @return a \eqn{2^k} by \eqn{2^k} transition matrix
#'
#' @examples
#' Msm_A(10.91, 0.11, 2)
#'
#' @export
Msm_A <- function(b,gamma.kbar,kbar){


  gamma.k    <- matrix(0,kbar,1)
  gamma.k[1] <- 1-(1-gamma.kbar)^(1/(b^(kbar-1)))


  A <- matrix(c(1-gamma.k[1] + .5*gamma.k[1],.5*gamma.k[1]
                ,.5*gamma.k[1], 1-gamma.k[1] + .5*gamma.k[1] ),2,2)
  if (kbar > 1) {

    for (i in 2:kbar) {
      gamma.k[i] <- 1-(1-gamma.k[1])^(b^(i-1))

      a          <- matrix(c(1-gamma.k[i] + .5*gamma.k[i],.5*gamma.k[i]
                    ,.5*gamma.k[i], 1-gamma.k[i] + .5*gamma.k[i] ),2,2)

      A          <- kronecker(A,a)
    }


  }


  return(A)
}
