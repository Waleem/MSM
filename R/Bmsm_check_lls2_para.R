#' Internal Routine for \code{\link{Bmsm}} Model.
#'
#' Internal routine used by \code{\link{Bmsm_std_err}} model, to check that the
#' hessian and gradient calculations conform with the Bmsm parameter conditions.
#'
#' @param x is a vector of \code{\link{Bmsm}} parameters.
#'
#' @return a 3 by 1 vector of \code{\link{Bmsm}} parameters.
#'
Bmsm_check_lls2_para <- function(x){

  if (x[1]>=1)  x[1] <- 0.9999
  if (x[1]<=-1) x[1] <- -0.9999

  if (x[2]>=1)  x[2] <- 0.9999

  if (x[3]>=1)  x[3] <- 0.9999
  if (x[3]<=-1) x[3] <- -0.9999

  return(x)
}
