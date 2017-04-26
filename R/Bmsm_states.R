#' Volatility State Vector For \code{\link{Bmsm}} model.
#'
#' Calculates all possible \eqn{4^k} state values for an  \code{\link{Bmsm}}(k) model.
#'
#' @param m01 is the state variable value for series 1.
#' @param m02 is the state variable value for series 2.
#' @param kbar is the number of frequency components in the \code{\link{Bmsm}}(k) model.
#'
#' @return a  2-by-\eqn{4^k} state matrix.
#'
#' @examples
#' s <- Bmsm_states(1.5, 1.7, 2)
#'
#' @export
Bmsm_states <- function(m01, m02, kbar){

  m <- list()

  for (i in seq(1,2*kbar-1,2)) {
    m[[i]] <- c(m02, 2-m02)
    m[[i+1]] <- c(m01, 2-m01)
  }

  m <- as.matrix(expand.grid(m))

  m.col <- ncol(m)/2
  k2    <- 2^kbar

  M1 <- matrix(m[,seq(2*kbar,1,-2)], nrow=4^kbar)
  M2 <- matrix(m[,seq(2*kbar-1,1,-2)], nrow=4^kbar)

  g1 <- apply(M1,1,prod)
  g2 <- apply(M2,1,prod)

  gm <- sqrt(rbind(g1,g2))

  return(gm)


}
