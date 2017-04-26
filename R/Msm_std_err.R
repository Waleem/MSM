#' Standard Error for \code{\link{Msm}} Model.
#'
#' Calculates standard error for \code{\link{Msm}} model.
#'
#' @param para is a vector of parameters returned by \code{\link{Msm}}.
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#' @param ret is a column matrix of returns.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' @param lag is the number of lags.
#'
#' @return a list of standard error ingredients.
#'
#' @export
Msm_std_err <- function(para, kbar, ret, n.vol,lag=0){

  if (kbar==1){
    grad <- Msm_grad(para,kbar,ret, n.vol)
    grad <- grad[,-2]
    J    <- t(grad) %*% grad
    s    <- sqrt(diag(solve(J)))
    se <- matrix(c(s[1], NA, s[2:3]),ncol=1)
  } else {

    VCV <- Msm_varcovvar(para, kbar, ret, n.vol,lag)
    se  <- matrix(sqrt(diag(VCV$VCV)), ncol=1)
  }

  return(se)
}
