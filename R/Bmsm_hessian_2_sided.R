#' Finite Difference Hessian for \code{\link{Bmsm}}(k) model.
#'
#' Calculates 2-sided finite difference hessian for \code{\link{Bmsm}}(k) model.
#'
#' @param func is a function handle.
#' @param arg.list is a list of arguments for function "func".
#'
#' @return a  matrix of numerical hessians.
#'
#'
#' @export
Bmsm_hessian_2_sided <-function (func, arg.list){

  para <- arg.list$para
  kbar <- arg.list$kbar
  dat  <- arg.list$dat
  n.vol<- arg.list$n.vol

  new.arg1 <- arg.list
  new.arg2 <- arg.list

  eps <- .Machine$double.eps

  para.size <- length(para)
  para      <- as.matrix(para)

  #f.ll <- -Msm_likelihood2(check_para(para), kbar, dat, n.vol)$LL
  f.ll <- do.call(func, arg.list)

  h <- matrix(eps^(1/3)*apply(cbind(abs(para),1e-8),1,max))

  para.h <- para+h

  h <- para.h-para

  ee <- diag(h[,1], para.size)

  gp <- matrix(0,para.size,1)
  gm <- matrix(0,para.size,1)

  for (i in 1:para.size){
    new.arg1$para <- para+ee[,i]
    new.arg2$para <- para-ee[,i]

    gp[i] <- do.call(func, new.arg1)
    gm[i] <- do.call(func, new.arg2)
    #gp[i] <- -Msm_likelihood2(check_para(para+ee[,i]), kbar, dat, n.vol)$LL
    #gm[i] <- -Msm_likelihood2(check_para(para-ee[,i]), kbar, dat, n.vol)$LL
  }

  hh <- h%*%t(h)
  hm <- matrix(0,para.size,para.size)
  hp <- matrix(0,para.size,para.size)

  for (i in 1:para.size){
    for (j in 1:para.size){

      new.arg1$para <- para+ee[,i]+ee[,j]
      new.arg2$para <- para-ee[,i]-ee[,j]

      hp[i,j] <- do.call(func, new.arg1)
      hp[j,i] <- hp[i,j]
      hm[i,j] <- do.call(func, new.arg2)
      hm[j,i] <- hm[i,j]
      #hp[i,j] <- -Msm_likelihood2(check_para(para+ee[,i]+ee[,j]), kbar, dat, n.vol)$LL
      #hm[i,j] <- -Msm_likelihood2(check_para(para-ee[,i]-ee[,j]), kbar, dat, n.vol)$LL
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
