#' Calculates the matrix of volatility components.
#'
#' Calculates the matrix of volatility components for \code{\link{Msm}}(k) model.
#'
#' @param m0 is the value of each volatility components and must be in the range (1, 2].
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#'
#' @return a \eqn{2^k} by \eqn{k} grid of volatility components.
#'
#' @examples
#' Msm_clustermat(1.5, 2)
#'
#' @export
Msm_clustermat <- function(m0,kbar){

  m.list <- list()

  for(k in 1:kbar){

    m.list[[k]] <- c(m0,2-m0)
  }

  M.mat <- expand.grid(m.list)
  M.mat <- M.mat[, rev(seq_len(ncol(M.mat)))]

  return(M.mat)
}
