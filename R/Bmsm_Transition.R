#' Internal Routine Used By \code{\link{Bmsm}} Model.
#'
#' Internal routine used by \code{\link{Bmsm}} model to calculate transition matrix.
#'
#' @param gama is the transition probability of the kth frequency component and must be in the range (0,1).
#' @param lamda in [0, 1] is the unconditional correlation between volatility arrivals.
#' @param rho.m in [-1, 1] is the correlation between volatility components \eqn{M_a} and \eqn{M_b}
#'
#'
#' @return 4-by-4 matrix.
#'
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
