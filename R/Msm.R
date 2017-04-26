#' Univariate Markov Switching Multifractal (MSM) Model.
#'
#' Estimates the parameters of a univariate Markov Switching Multifractal (MSM) model for a given vector of returns.
#'
#' @param ret is a column matrix of returns.
#' @param kbar is the number of frequency components in the Msm(k) model. Default is 1.
#' @param n.vol is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' If data is monthly, then set n.vol to 12. If data is quarterly, then n.vol should be 4.
#' @param para0 is the initial parameter values for optimization. Default is NULL.
#' @param nw.lag is the number of lags to use for calculating robust Newey-West standard errors. Default is 2.
#'
#' @return a list consisting of:
#' \item{LL}{sum of loglikelihood values at optimum}
#' \item{LLs}{a vector of loglikelihood values at optimum}
#' \item{filtered}{a matrix of filtered probabilities}
#' \item{A}{the estimated transition matrix}
#' \item{g.m}{the estimated state values}
#' \item{optim.msg}{optimization message}
#' \item{optim.convergence}{optimization convergence indicator}
#' \item{optim.iter}{number of iterations}
#' \item{para]}{estimated parameter vector}
#' \item{se}{robust standard errors}
#'
#' @author Waleem Alausa, \email{alausa.babs@@gmail.com}
#' @keywords Markov Switching Multifractal, volatility, volatility clustering
#'
#' @note Use the \code{\link{summary.msmmodel}} function to print summary results of the MSM model.
#' Use \code{\link{plot.msmmodel}} function to plot the fitted conditional volatilities, or plot(msmmodel, what = "volsq") to plot the
#'  fitted  conditional variance. Use \code{\link{predict.msmmodel}} to get the predicted conditional volatility and variance.
#'
#'
#'
#' @examples
#' data("calvet2004data")
#' ret <- na.omit(as.matrix(calvet2004data$caret))*100
#' fit <- Msm(ret)
#' fit2 <- Msm(ret, kbar=2, n.vol=252, nw.lag=2)
#' summary(fit2)
#' plot(fit2)
#'
#' @export
Msm <- function(ret, kbar =1 , n.vol = 252, para0=NULL, nw.lag =0){

  msm.check <- Msm_parameter_check(ret, kbar, para0)

  ret  <- msm.check$dat
  kbar <- msm.check$kbar
  x0   <- msm.check$start.value
  lb   <- msm.check$lb
  ub   <- msm.check$ub

  log <- capture.output({
    msm.fit <- Rsolnp::solnp(x0, fun = Msm_ll2, LB = lb, UB = ub, kbar = kbar, dat = ret, n.vol = n.vol)
  })


  msm.estimate <- Msm_likelihood2(msm.fit$par,kbar,ret,n.vol)
  se           <- Msm_std_err(msm.fit$par,kbar,ret,n.vol,nw.lag)

  para <- matrix(msm.fit$par,ncol=1)

  if (kbar==1) para[2]=NA
  # Parameters  <- cbind(para, se)
  # colnames(Parameters) <- c("Parameters", "Std. Error")
  coef    <- para
  coef[4] <- coef[4]/sqrt(n.vol)
  se[4]   <- se[4]/sqrt(n.vol)

  rownames(coef) <- c("m0", "b", "gammak", "sigma")
  colnames(coef) <- "Estimate"
  colnames(se)   <- "Std. Error"


  msm.estimate$optim.msg <- msm.fit$message
  msm.estimate$optim.convergence <- msm.fit$convergence
  msm.estimate$optim.iter <- msm.fit$iter
  msm.estimate$para <- para
  msm.estimate$se   <- se
  msm.estimate$kbar <- kbar
  msm.estimate$n    <- n.vol
  msm.estimate$coefficients <- coef
  msm.estimate$call   <- match.call()
  msm.estimate$ret <- ret



  #return(msm.estimate)
  class(msm.estimate) <- "msmmodel"
  msm.estimate
}

###################################################################################
msmmodel <- function(x, ...) UseMethod("msmmodel")
# msmmodel.default <- function(x, ...){
#
#   x$df     <- nrow(x$ret)-4
#   x
#
# }

print.msmmodel <- function(x, ...) {

  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients, digits=4)
  cat("\n")
  cat("LogLikelihood:", x$LL)
}

summary.msmmodel <- function(object, ...)
{
  se      <- object$se
  tval    <- coef(object) / se
  p.value <- 2*pt(-abs(tval), df=nrow(object$ret)-4)

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
  class(res) <- "summary.msmmodel"
  res
}

print.summary.msmmodel <- function(x, ...)
{
  #cat("Call:\n")
  cat("*----------------------------------------------------------------------------*\n")
  cat("  Markov Switching Multifractal Model With", x$kbar, "Volatility Component(s) \n")
  cat("*----------------------------------------------------------------------------*\n")
  cat("\n")
  printCoefmat(x$coefficients, digits=4, P.value=TRUE, has.Pvalue=TRUE)
  cat("\nLogLikelihood:", x$LL)
}

predict.msmmodel <- function(object, h=NULL,...){


  if (!is.null(h)){
    smoothed.p = object$filtered
  } else{
    smoothed.p <- Msm_smooth_cpp(object$A,object$filtered)
  }

  pred <- Msm_predict(object$g.m, object$para[4], object$n, smoothed.p, object$A, h)
  return(pred)
}

plot.msmmodel <- function(object, what="vol", ...){

  #pred <- predict.msm(object)
  smoothed.p <- Msm_smooth_cpp(object$A,object$filtered)
  pred       <- Msm_predict(object$g.m, object$para[4], object$n, smoothed.p, object$A)

  if(what=="vol"){
    plot.df = matrix(cbind(pred$vol, abs(object$ret)), ncol=2)
    colnames(plot.df) <- c("Conditional Volatility", "Absolute Returns")
    plot.df <- reshape2::melt(plot.df)

    msm_plot <- ggplot2::ggplot(plot.df, ggplot2::aes(x=Var1, y=value, colour=Var2)) +
      ggplot2::geom_line() + ggplot2::xlab("Time") +
      ggplot2::ylab("Volatility") + ggplot2::ggtitle("Conditional Volatility vs Absolute Returns") +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position="bottom")

  }else if(what=="volsq"){
    plot.df = matrix(cbind(pred$vol.sq, object$ret^2), ncol=2)
    colnames(plot.df) <- c("Conditional Variance", "Squared Returns")
    plot.df <- reshape2::melt(plot.df)

    msm_plot <- ggplot2::ggplot(plot.df, ggplot2::aes(x=Var1, y=value, colour=Var2)) +
      ggplot2::geom_line() + ggplot2::xlab("Time") +
      ggplot2::ylab("Variance") + ggplot2::ggtitle("Conditional Variance vs Squared Returns") +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position="bottom")

  }else{stop('what must be either vol or volsq')}

  #class(msm_plot) <- "plot.msm"
  print(msm_plot)


}
