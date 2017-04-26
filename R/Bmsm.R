#' Bivariate Markov Switching Multifractal (BMSM) Model.
#'
#' Estimates the parameters of a bivariate MSM model for a given pair of returns.
#'
#' @param ret is a matrix of two returns.
#' @param kbar is the number of frequency components in the bivariate MSM model. Default is 1.
#' @param n is the number of trading days in a year, if data is on a daily frequency. Default is 252.
#' If data is monthly, then set n.vol to 12. If data is quarterly, then n.vol should be 4.
#' @param para0 is the initial parameter values for optimization,
#' c(m01, m02, sigma1, sigma2, gamma-k, b, rho.e, labda, rho.m). Default is NULL.
#' @param s.err =T. If True, estimates standard error. Otherwise, no standard errors are estimated.
#' The computation of BMSM standard error is expensive. Hence, the option to skip it if not needed.
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
#' @note Use the \code{\link{summary.bmsmmodel}} function to print summary results of the BMSM model.
#' Use \code{\link{plot.bmsmmodel}} function to plot the fitted conditional volatilities, or plot(bmsmmodel) to plot the
#'  fitted  conditional variance. Use \code{\link{predict.bmsmmodel}} to get the predicted conditional
#'  variance, standard deviation, correlation and covariance.
#'
#' @references Calvet, L.E & Fisher, J.A (2006) Volatility comovement: a multifrequency approach
#' (\href{http://www.sciencedirect.com/science/article/pii/S0304-4076(05)00010-2}{PubMed})
#'
#' @examples
#' data("calvet2006returns")
#' ret <- as.matrix(calvet2006returns[,2:3])*100
#' fit <- Bmsm(ret)
#' fit <- Bmsm(ret, kbar=2, n=252, s.err=FALSE)
#' summary(fit)
#' plot(fit)
#'
#' @export
Bmsm <- function(ret, kbar =1, n = 252, para0=NULL, s.err = TRUE){

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
    stage1.fit <- Rsolnp::solnp(para1, fun = Bmsm_stage1_likelihood,
                                LB = lb1, UB = ub1, dat = ret, kbar = kbar, n.vol = n)

    para1 <- stage1.fit$par

    stage2.fit <- Rsolnp::solnp(para2, fun = Bmsm_stage2_likelihood2,
                                LB = lb2, UB = ub2, kbar = kbar, dat = ret, para1=para1, n.vol = n)

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

  if (!is.null(h)){
    smoothed.p = object$filtered.P
  } else{
    smoothed.p <- Msm_smooth_cpp(object$A,object$filtered.P)
  }

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
