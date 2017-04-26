#' Marginal Probabilities for \code{\link{Bmsm}} components.
#'
#' Calculates the matrix of marginal probabilities for \code{\link{Bmsm}}(k) volatility components.
#'
#' @param smoothed.p is matrix of smoothed/filtered \code{\link{Bmsm}} state probabilities.
#' See also \code{\link{Msm}}, \code{\link{Msm_likelihood2}}.
#' @param m01 is the volatility state value for return series 1 in (1, 2].
#' @param m02 is the volatility state value for return series 2 in (1, 2].
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#'
#' @return a list consisting of:
#' \item{palpha}{marginal probabilities for volatility state vectors of series 1}
#' \item{pbeta}{marginal probabilities for volatility state vectors of series 2}
#' \item{Malpha}{\eqn{2^k} by \eqn{k} grid of \code{\link{Msm}} volatility components for series 1}
#' \item{Mbeta}{\eqn{2^k} by \eqn{k} grid of \code{\link{Msm}} volatility components for series 2}
#'
#' @examples
#' data("calvet2006returns")
#' ret <- as.matrix(calvet2006returns[,2:3])*100
#' fit <- Bmsm(ret, kbar=2, n=252, s.err=FALSE)
#' marg <- Bmsm_marginals(fit$filtered, fit$para[1], fit$para[2],2)
#'
#' @export
Bmsm_marginals <- function(smoothed.p,m01,m02,kbar){


  Malpha <- Msm_clustermat(m01,kbar)
  Mbeta  <- Msm_clustermat(m02,kbar)
  M      <- Bmsm_clustermat(m01,m02,kbar)

  M1 <- M[,1:kbar]
  M2 <- M[,(kbar+1):ncol(M)]

  palpha <- matrix(0,nrow(smoothed.p),2^kbar)
  pbeta  <- matrix(0,nrow(smoothed.p),2^kbar)

  for (i in 1:(2^kbar)) {

    col.sel <- apply(M1 == (matrix(rep(Malpha[i,],4^kbar),nrow=4^kbar,byrow=T)), 1, all)
    palpha[,i]  <- apply(matrix(smoothed.p[,col.sel],ncol=sum(col.sel==TRUE)),1,sum)

    col.sel <- apply(M2 == (matrix(rep(Mbeta[i,],4^kbar),nrow=4^kbar,byrow=T)), 1, all)
    pbeta[,i]   <- apply(matrix(smoothed.p[,col.sel],ncol=sum(col.sel==TRUE)),1,sum)

  }

  marginals <- list(
    palpha = palpha,
    pbeta  = pbeta,
    Malpha = Malpha,
    Mbeta  = Mbeta
  )
  return(marginals)

}
