#' Prediction for \code{\link{Bmsm}} Model.
#'
#' Performs prediction after fitting a \code{\link{Bmsm}}(k) model.
#'
#' @param smoothed.p is a matrix of smoothed/filtered state probabilities.
#' @param A is the transition matrix.
#' @param g.m is is a vector of possible \code{\link{Bmsm}} states.
#' @param para is a vector of \code{\link{Bmsm}} parameters.
#' @param kbar is the number of volatility components in \code{\link{Bmsm}}.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' If data is monthly, then set n.vol to 12. If data is quarterly, then n.vol should be 4.
#' @param h is an optional parameter. When h is null, the fitted volatility values are returned.
#' When h>0, then an h-step ahead volatility forecast is returned.
#'
#' @return a list consisting of:
#' \item{vol1}{fitted/forecasted volatility values for series 1}
#' \item{vol2}{fitted/forecasted volatility values for series 2}
#' \item{vol1.sq}{fitted/forecasted variance values for series 1}
#' \item{vol2.sq}{fitted/forecasted variance values for series 2}
#' \item{rho.t}{fitted/forecasted conditional correlation}
#' \item{covt}{fitted/forecasted conditional covariance}
#'
#' @examples
#' data("calvet2006returns")
#' ret <- as.matrix(calvet2006returns[,2:3])*100
#' fit <- Bmsm(ret, kbar=2, n=252, s.err=FALSE)
#' v=Bmsm_predict(fit$filtered.P, fit$A, fit$g.m, fit$para, fit$kbar, fit$n)
#'
#' @export
Bmsm_predict <- function(smoothed.p, A, g.m, para, kbar, n.vol, h =NULL){


  if (!is.null(h) && h<1) stop("h must be a non-zero integer")
  if (!is.null(h)) h <- floor(h)

  m01     <- para[1]
  m02     <- para[2]
  sigma1  <- para[3]/sqrt(n.vol)
  sigma2  <- para[4]/sqrt(n.vol)
  gamma.k <- para[5]
  b       <- para[6]
  rho.e   <- para[7]
  lamda   <- para[8]
  rho.m   <- para[9]

  if (!is.null(h)) smoothed.p <- smoothed.p[nrow(smoothed.p),] %*% Msm_mat_power(A,h)

  marginals <- Bmsm_marginals(smoothed.p, m01,m02, kbar)
  #g.m       <- Bmsm_states(m01, m02, kbar)

  palpha <- marginals$palpha
  pbeta  <- marginals$pbeta
  Malpha <- marginals$Malpha
  Mbeta  <- marginals$Mbeta

  rho.t <- rho.e*(smoothed.p %*% matrix(apply(g.m,2,prod),ncol=1)) /
        sqrt( (palpha %*% matrix(apply(Malpha,1,prod), ncol=1)) * (pbeta %*% matrix(apply(Mbeta,1,prod), ncol=1)) )

  cov.t  <- sigma1*sigma2*rho.e * (smoothed.p %*% matrix(apply(g.m,2,prod),ncol=1))
  var1.t <- sigma1^2 * (palpha %*% matrix(apply(Malpha,1,prod), ncol=1))
  var2.t <- sigma2^2 * (pbeta  %*% matrix(apply(Mbeta,1,prod), ncol=1))
  #std1.t <- (sigma1) * (palpha %*% sqrt(matrix(apply(Malpha,1,prod), ncol=1)))
  #std2.t <- (sigma2) * (pbeta  %*% sqrt(matrix(apply(Mbeta,1,prod), ncol=1)))

  covar <- list(
    vol1    = sqrt(var1.t),
    vol2    = sqrt(var2.t),
    covt    = cov.t,
    rho.t   = rho.t,
    vol1.sq = var1.t,
    vol2.sq = var2.t
  )

  return(covar)


}
