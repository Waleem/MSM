#' Decomposes returns into volatility components.
#'
#' Decomposes returns into \eqn{k>1} volatility components.
#'
#' @param msmobject is an object of class "msmmodel" returned by \code{\link{Msm}}.
#'
#' @return An object of class msmcomp containing the \eqn{T} by \eqn{k} matrix of volatility components, \eqn{E(M)}.
#'  The returned object can be coarsed into a matrix with \code{\link{as.matrix}} function.
#'  Use \code{\link{plot.msmmodel}} to plot the volatility components.
#'
#' @author Waleem Alausa, \email{alausa.babs@@gmail.com}
#' @keywords Markov Switching Multifractal, volatility, volatility clustering
#'
#' @examples
#' data("calvet2004data")
#' ret <- na.omit(as.matrix(dat$caret))*100
#' fit2 <- Msm(ret, kbar=2, n.vol=252, nw.lag=2)
#' volcomp <- Msm_decompose(fit2)
#' plot(volcomp)
#'
#'
#' @export

Msm_decompose <- function(msmobject){

  if (!class(msmobject) == "msmmodel") stop('object is not an MSM model')

  if (msmobject$kbar < 2) stop('k (number of volatility components) must be > 1')

  kbar <- msmobject$kbar
  m0   <- msmobject$para[1]
  p  <- Msm_smooth_cpp(msmobject$A,msmobject$filtered)


  m.mat <- as.matrix(Msm_clustermat(m0,kbar))

  m.marginals <- Msm_marginals(p,m0,m.mat,kbar)

  em <- m0*m.marginals + (2-m0)*(1-m.marginals)

  # if (isTRUE(graph)) {
  #   colnames(em) <- paste("M",seq.int(1,kbar,1))
  #   em.long <- reshape2::melt(em)
  #
  #   m.plot <- ggplot2::ggplot(em.long, aes(x=Var1, y=value)) +
  #     ggplot2::geom_line() + ggplot2::guides(colour=FALSE) + ggplot2::xlab("Time") +
  #     ggplot2::ylab("M") + ggplot2::ggtitle("Volatility Components")
  #
  #   m.plot <- m.plot + ggplot2::aes(colour=factor(Var2))
  #   m.plot <- m.plot + ggplot2::facet_wrap(~ Var2, ncol=1, scales = "free")
  #   print(m.plot)
  # }
  class(em) <- "msmcomp"

  em
}

###################################################################################

msmcomp <- function(x, ...) UseMethod("msmcomp")

as.matrix.msmcomp <- function (x,...){
  class(x) <- "matrix"
  colnames(x) <- paste("M",seq.int(1,ncol(x),1))
  x
}
plot.msmcomp <- function(x, ...){

  em.long <- reshape2::melt(as.matrix(x))

  m.plot <- ggplot2::ggplot(em.long, ggplot2::aes(x=Var1, y=value)) +
    ggplot2::geom_line() + ggplot2::guides(colour=FALSE) + ggplot2::xlab("Time") +
    ggplot2::ylab("M") + ggplot2::ggtitle("MSM Volatility Components")

  m.plot <- m.plot + ggplot2::aes(colour=factor(Var2))
  m.plot <- m.plot + ggplot2::facet_wrap(~ Var2, ncol=1, scales = "free")

  print(m.plot)

}
