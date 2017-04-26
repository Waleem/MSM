#' Finite Difference Hessian for \code{\link{Msm}}(k) model.
#'
#' Calculates 2-sided finite difference hessian for \code{\link{Msm}}(k) model.
#'
#' @param para is a vector of parameters returned by \code{\link{Msm}}.
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#' @param ret is a column matrix of returns.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#'
#' @return a \eqn{4 x 4} matrix of numerical hessians.
#'
#' @export
Msm_hesssian_2_sided <-function (para, kbar, ret, n.vol){

  check_para <- function(x){
    if (x[1]>=2) x[1] <- 1.9999
    if (x[3]>=1) x[3] <- 0.9999

    return(x)
  }

  eps <- .Machine$double.eps

  para.size <- length(para)
  para      <- as.matrix(para)

  f.ll <- Msm_likelihood2(check_para(para), kbar, ret, n.vol)$LL

  h <- matrix(eps^(1/3)*apply(cbind(abs(para),1e-8),1,max))

  para.h <- para+h

  h <- para.h-para

  ee <- diag(h[,1], para.size)

  gp <- matrix(0,para.size,1)
  gm <- matrix(0,para.size,1)

  for (i in 1:para.size){
    gp[i] <- Msm_likelihood2(check_para(para+ee[,i]), kbar, ret, n.vol)$LL
    gm[i] <- Msm_likelihood2(check_para(para-ee[,i]), kbar, ret, n.vol)$LL
  }

  hh <- h%*%t(h)
  hm <- matrix(0,para.size,para.size)
  hp <- matrix(0,para.size,para.size)

  for (i in 1:para.size){
    for (j in 1:para.size){

      hp[i,j] <- Msm_likelihood2(check_para(para+ee[,i]+ee[,j]), kbar, ret, n.vol)$LL
      hp[j,i] <- hp[i,j]
      hm[i,j] <- Msm_likelihood2(check_para(para-ee[,i]-ee[,j]), kbar, ret, n.vol)$LL
      hm[j,i] <- hm[i,j]
    }
  }

  H <- matrix(0,para.size, para.size)

  for (i in 1:para.size){
    for (j in 1:para.size){

      H[i,j] <- (hp[i,j]-gp[i]-gp[j]+f.ll+f.ll-gm[i]-gm[j]+hm[i,j]) / hh[i,j] /2
      H[j,i] <- H[i,j]

    }
  }

  return(H)
}
