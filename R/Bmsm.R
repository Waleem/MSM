#
Bmsm <- function(ret, kbar =1, n = 252, para0=NULL, s.err = T){

  bmsm.check <- Bmsm_parameter_check(ret, kbar, para0, n)

  ret  <- bmsm.check$dat
  kbar <- bmsm.check$kbar
  x0   <- bmsm.check$start.value
  lb1  <- bmsm.check$lb1
  ub1  <- bmsm.check$ub1
  lb2  <- bmsm.check$lb2
  ub2  <- bmsm.check$ub2

  para1 <- x0[1:6]
  para2 <- x0[7:9]

  log <- capture.output({
    stage1.fit <- nloptr::slsqp(para1, fn = Bmsm_stage1_likelihood,
                                lower = lb1, upper = ub1, dat = ret, kbar = kbar, n.vol = n)

    para1 <- stage1.fit$par

    stage2.fit <- nloptr::slsqp(para2, fn = Bmsm_stage2_likelihood2,
                                lower = lb2, upper = ub2, kbar = kbar, dat = ret, para1=para1, n.vol = n)

  })

  para <- matrix(c(para1,stage2.fit$par),ncol=1)

  bmsm.estimate <- Bmsm_filtered2(c(para1,stage2.fit$par),kbar,ret,n)

  se <- matrix(NA,9,1)

  if (isTRUE(s.err))  se <- Bmsm_std_err(para,kbar,ret,n)

  if (kbar==1) para[6]=NA
  coef    <- para
  coef[3:4] <- coef[3:4]/sqrt(n)
  se[3:4]   <- se[3:4]/sqrt(n)

  rownames(coef) <- c("m01", "m02", "sigma1", "sigma2", "gammak", "b", "rhoe", "lambda", "rhom")
  colnames(coef) <- "Estimate"
  colnames(se)   <- "Std. Error"

  bmsm.estimate$optim.msg <- c(stage1.message=stage1.fit$message,stage2.message=stage2.fit$message)
  bmsm.estimate$optim.convergence <- c(stage1.convergence=stage1.fit$convergence,
                                       stage2.convergence=stage2.fit$convergence)
  bmsm.estimate$optim.iter <- c(stage1.iteration=stage1.fit$iter,stage2.iteration=stage2.fit$iter)
  bmsm.estimate$coefficients <- coef
  bmsm.estimate$call   <- match.call()
  bmsm.estimate$ret <- ret
  bmsm.estimate$LL1 <- stage1.fit$value
  bmsm.estimate$se   <- se

  #return(bmsm.estimate)
  class(bmsm.estimate) <- "bmsmmodel"
  bmsm.estimate
}

###################################################################################
bmsmmodel <- function(x, ...) UseMethod("bmsmmodel")

print.bmsmmodel <- function(x, ...) {

  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients, digits=4)
  cat("\n")
  cat("LogLikelihood:", x$LL)
}

summary.bmsmmodel <- function(object, ...)
{
  se      <- object$se
  tval    <- coef(object) / se
  p.value <- 2*pt(-abs(tval), df=nrow(object$ret)-9)

  colnames(tval)      <- "t.value"
  colnames(p.value)   <- "p.value"

  TAB  <- cbind(Estimate = round(coef(object),4),
                StdErr    = round(se,4),
                t.value   = round(tval,4),
                p.value   = round(p.value,4))

  res <- list(call        =object$call,
              coefficients=TAB,
              kbar        =object$kbar,
              LL          =object$LL)
  class(res) <- "summary.bmsmmodel"
  res
}

print.summary.bmsmmodel <- function(x, ...)
{
  #cat("Call:\n")
  cat("*-------------------------------------------------------------------------------------*\n")
  cat("  Bivariate Markov Switching Multifractal Model With", x$kbar, "Volatility Component(s) \n")
  cat("*-------------------------------------------------------------------------------------*\n")
  cat("\n")
  printCoefmat(x$coefficients, digits=4, P.value=TRUE, has.Pvalue=TRUE)
  cat("\nLogLikelihood:", x$LL)
}

predict.bmsmmodel <- function(object, h=NULL,...){
  smoothed.p <- Msm_smooth_cpp(object$A,object$filtered.P)
  pred       <- Bmsm_predict(smoothed.p, object$A, object$g.m, object$para, object$kbar, object$n, h)
  return(pred)
}

plot.bmsmmodel <- function(object, ...){

  #pred <- predict.msm(object)
  smoothed.p <- Msm_smooth_cpp(object$A,object$filtered.P)
  pred       <- Bmsm_predict(smoothed.p, object$A, object$g.m, object$para, object$kbar, object$n)


    plot.df = data.frame(matrix(unlist(pred[1:4]), nrow = nrow(object$ret)))
    colnames(plot.df) <- c("Conditional Volatility - Series1 ", "Conditional Volatility - Series2 ",
                           "Conditional Covariance", "Conditional Correlation")
    plot.df <- reshape2::melt(as.matrix(plot.df))

    bmsm_plot <- ggplot2::ggplot(plot.df, ggplot2::aes(x=Var1, y=value)) +
      ggplot2::geom_line() + ggplot2::guides(colour=FALSE) + ggplot2::xlab("Time") +
      ggplot2::ggtitle("Volatility Commovement")

    bmsm_plot <- bmsm_plot + ggplot2::aes(colour=factor(Var2))
    bmsm_plot <- bmsm_plot + ggplot2::facet_wrap(~ Var2, ncol=1, scales = "free")
      #ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position="bottom")



  print(bmsm_plot)


}
