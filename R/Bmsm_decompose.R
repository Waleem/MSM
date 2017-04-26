#' Internal Routine Used By \code{\link{Bmsm}} Model.
#'
#'
#' @param smoothed.p is a matrix of smoothed probabilities from \code{\link{Bmsm}} Model.
#' @param m01 is the volatility state value for return series 1 in (1, 2].
#' @param m02 is the volatility state value for return series 2 in (1, 2].
#' @param kbar is the number of frequency components in the \code{\link{Bmsm}}(k) model.
#'
#' @return decomposed volatility components.
#'
Bmsm_decompose <- function(smoothed.p, m01, m02, kbar) {


    marginals <- Bmsm_marginals(smoothed.p, m01, m02, kbar)

    palpha <- marginals$palpha
    pbeta <- marginals$pbeta

    em1 <- m01 * palpha + (2 - m01) * (1 - palpha)
    em2 <- m02 * pbeta + (2 - m01) * (1 - pbeta)

    em <- list(em1=em1, em2=em2)
}
