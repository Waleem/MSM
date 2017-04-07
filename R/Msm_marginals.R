#' Calculates the matrix of marginal probabilities for \code{\link{Msm}} components.
#'
#' Calculates the matrix of marginal probabilities for \code{\link{Msm}}(k) volatility components.
#'
#' @param p is matrix of smoothed/filtered \code{\link{Msm}} state probabilities.
#' See also \code{\link{Msm}}, \code{\link{Msm_likelihood2}}.
#' @param m is the value of each volatility components and must be in the range (1, 2].
#' @param Mmat is a matrix of volatility components. See also \code{\link{Msm_clustermat}}
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#'
#' @return a \eqn{N=nrow(p)} by \eqn{k} matrix of marginal probabilities.
#'
#' @examples
#' data("calvet2004data")
#' ret <- na.omit(as.matrix(dat$caret))*100
#' kbar <- 2
#' fit <- Msm(ret, kbar, n.vol=252, nw.lag=2)
#' mmat <- Msm_clustermat(fit$para[1], kbar)
#' marg <- Msm_marginals(fit$filtered,fit$para[1],mmat,kbar)
#'
#' @export


Msm_marginals <- function(p,m,Mmat,kbar){

  Mmat <- t(Mmat)

  m.marginals <- matrix(0,nrow(p),kbar)

  for(k in 1:kbar){

    col.sel <- Mmat[k,]==m

    m.marginals[,k] <- apply(p[,col.sel],1,sum)

  }
  return(m.marginals)
}
