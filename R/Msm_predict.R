#' Performs prediction for MSM model.
#'
#' Performs prediction after fitting an \code{\link{Msm}}(k) model.
#'
#' @param g.m is is a vector of possible msm states.
#' @param sigma is the unconditional volatility returned by \code{\link{Msm}}(k) model.
#' @param n is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' If data is monthly, then set n.vol to 12. If data is quarterly, then n.vol should be 4.
#' @param P is a matrix of smoothed/filtered state probabilities.
#' @param A is the transition matrix returned by Msm.
#' @param h is an optional parameter. When h is null, the fitted volatility values are returned.
#' When h>0, then an h-step ahead volatility forecast is returned.
#'
#' @return a list consisting of:
#' \item{vol}{fitted/forecasted volatility values}
#' \item{LLs}{fitted/forecasted variance values}
#'
#' @examples
#' data("calvet2004data")
#' ret <- na.omit(as.matrix(dat$caret))*100
#' fit <- Msm(ret, kbar=2, n.vol=252, nw.lag=2)
#' Msm_predict(fit$g.m, fit$para[4]*sqrt(252), 252, fit$filtered, fit$A, 5)
#' @export
Msm_predict <- function(g.m, sigma, n, P, A, h=NULL){

  if (!is.null(h) && h<1) stop("h must be a non-zero integer")

  sigma  <- sigma/sqrt(n)

  # vol.hat   <- NULL
  # volsq.hat <- NULL
  # p.hat     <- NULL
  vol       <- NULL
  vol.sq    <- NULL

  #if(!is.null(h) || !is.null(A)){
  if(!is.null(h)){

    p.hat  <- matrix(P[nrow(P),],1,ncol(P)) %*% A^h
    vol    <- sigma*p.hat%*% t(g.m)
    vol.sq <- sigma^2*(p.hat%*% t(g.m^2))

  }else{

    vol    <- sigma*(P %*% t(g.m))
    vol.sq <- sigma^2*(P %*% t(g.m^2))

  }

  prediction <- list(
    vol       = vol,
    vol.sq    = vol.sq
  )

  return(prediction)
}
