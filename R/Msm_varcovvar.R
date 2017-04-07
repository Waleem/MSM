#' Internal routine used to calculate robust newey-West standard error for MSM model.
#'
#' @param para is a vector of parameters returned by \code{\link{Msm}}.
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#' @param ret is a column matrix of returns.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' @param lag is the number of lags.
#'
#' @return a list of standard error ingredients.
#'
#'
Msm_varcovvar <- function(para, kbar, ret, n.vol,lag=0){

  check_para <- function(x){
    if (x[1]>=2) x[1] <- 1.9999
    if (x[3]>=1) x[3] <- 0.9999

    return(x)
  }

  cov.nw <- function(dat, nlag, demean=TRUE){

    t <- nrow(dat)
    if(demean){
      dat <- dat - apply(dat,2,mean)
    }
    w <- (nlag+1-(0:nlag))/(nlag+1)
    V <- (t(dat)%*%dat)/t

    for (i in 1:nlag) {
      g <- (t(dat[(i+1):t,]) %*% dat[1:(t-i),])/t
      j <- g+t(g)
      V <- V+(w[i+1]*j)
    }

    return(V)
  }

  para.size <- length(para)
  para      <- as.matrix(para)

  eps <- .Machine$double.eps

  h <- apply(cbind(abs(para*eps^(1/3)),1e-8),1,max)
  h <- diag(h)

  like <- Msm_likelihood2(check_para(para), kbar, ret, n.vol)$LLs

  t <- length(like)

  ll.fp <- matrix(0,para.size,1)
  ll.fm <- matrix(0,para.size,1)

  like.p <- matrix(0,t, para.size)
  like.m <- matrix(0,t, para.size)

  for (i in 1:para.size){
    para.ph    <- para+h[,i]
    p.list     <- Msm_likelihood2(check_para(para.ph), kbar, ret, n.vol)
    ll.fp[i]   <- -p.list$LL
    like.p[,i] <- p.list$LLs
    para.ph    <- para-h[,i]
    p.list     <- Msm_likelihood2(check_para(para.ph), kbar, ret, n.vol)
    ll.fm[i]   <- -p.list$LL
    like.m[,i] <- p.list$LLs

  }

  scores       <- matrix(0,t,para.size)
  gross.scores <- matrix(0,para.size,1)

  h <- diag(h)

  for (i in 1:para.size){
    scores[,i]       <- (like.p[,i] - like.m[,i])/(2*h[i])
    gross.scores[i] <- (ll.fp[i] - ll.fm[i])/(2*h[i])

  }

  H <- Msm_hesssian_2_sided(para,kbar,ret,n.vol)
  A <- H/t
  H <- A

  A.inv <- solve(A)
  if (lag==0){
    B   <- cov(scores)
    VCV <- (A.inv%*%B%*%A.inv)/t
  } else {
    B <- cov.nw(scores,lag)
    VCV <- (A.inv%*%B%*%A.inv)/t
  }
  varcovar <- list(
    VCV          = VCV,
    hessian      = H,
    gross.scores = gross.scores,
    scores       = scores,
    A            = A,
    B            = B
  )
  return(varcovar)
}

#' Internal routine used to calculate robust newey-West standard error for MSM model.
#'
#' @param para is a vector of parameters returned by \code{\link{Msm}}.
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#' @param ret is a column matrix of returns.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' @param lag is the number of lags.
#'
#' @return a list of standard error ingredients.
#'
#'
Msm_varcovvar <- function(para, kbar, ret, n.vol,lag=0){

  check_para <- function(x){
    if (x[1]>=2) x[1] <- 1.9999
    if (x[3]>=1) x[3] <- 0.9999

    return(x)
  }

  cov.nw <- function(dat, nlag, demean=TRUE){

    t <- nrow(dat)
    if(demean){
      dat <- dat - apply(dat,2,mean)
    }
    w <- (nlag+1-(0:nlag))/(nlag+1)
    V <- (t(dat)%*%dat)/t

    for (i in 1:nlag) {
      g <- (t(dat[(i+1):t,]) %*% dat[1:(t-i),])/t
      j <- g+t(g)
      V <- V+(w[i+1]*j)
    }

    return(V)
  }

  para.size <- length(para)
  para      <- as.matrix(para)

  eps <- .Machine$double.eps

  h <- apply(cbind(abs(para*eps^(1/3)),1e-8),1,max)
  h <- diag(h)

  like <- Msm_likelihood2(check_para(para), kbar, ret, n.vol)$LLs

  t <- length(like)

  ll.fp <- matrix(0,para.size,1)
  ll.fm <- matrix(0,para.size,1)

  like.p <- matrix(0,t, para.size)
  like.m <- matrix(0,t, para.size)

  for (i in 1:para.size){
    para.ph    <- para+h[,i]
    p.list     <- Msm_likelihood2(check_para(para.ph), kbar, ret, n.vol)
    ll.fp[i]   <- -p.list$LL
    like.p[,i] <- p.list$LLs
    para.ph    <- para-h[,i]
    p.list     <- Msm_likelihood2(check_para(para.ph), kbar, ret, n.vol)
    ll.fm[i]   <- -p.list$LL
    like.m[,i] <- p.list$LLs

  }

  scores       <- matrix(0,t,para.size)
  gross.scores <- matrix(0,para.size,1)

  h <- diag(h)

  for (i in 1:para.size){
    scores[,i]       <- (like.p[,i] - like.m[,i])/(2*h[i])
    gross.scores[i] <- (ll.fp[i] - ll.fm[i])/(2*h[i])

  }

  H <- Msm_hesssian_2_sided(para,kbar,ret,n.vol)
  A <- H/t
  H <- A

  A.inv <- solve(A)
  if (lag==0){
    B   <- cov(scores)
    VCV <- (A.inv%*%B%*%A.inv)/t
  } else {
    B <- cov.nw(scores,lag)
    VCV <- (A.inv%*%B%*%A.inv)/t
  }
  varcovar <- list(
    VCV          = VCV,
    hessian      = H,
    gross.scores = gross.scores,
    scores       = scores,
    A            = A,
    B            = B
  )
  return(varcovar)
}

