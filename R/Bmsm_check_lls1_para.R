#' Internal Routine for \code{\link{Bmsm}} Model.
#'
#' Internal routine used by \code{\link{Bmsm_std_err}} model, to check that the
#' hessian and gradient calculations conform with the Bmsm parameter conditions.
#'
#' @param x is a vector of \code{\link{Bmsm}} parameters.
#'
#' @return a 6 by 1 vector of \code{\link{Bmsm}} parameters.
#'

Bmsm_check_lls1_para <- function(x){
  if (x[1]>=2) x[1] <- 1.9999
  if (x[2]>=2) x[2] <- 1.9999
  if (x[5]>=1) x[5] <- 0.9999

  return(x)
}
