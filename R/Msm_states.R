#' Volatility State Vector For \code{\link{Msm}} model.
#'
#' Calculates all possible \eqn{2^k} state values for an  \code{\link{Msm}}(k) model.
#'
#' @param m0 is the state variable value.
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#'
#' @return a \eqn{2^k} state vector.
#'
#' @examples
#' Msm_states(1.5, 2)
#'
#' @export
Msm_states <- function(m0, kbar){
  m1  <- 2-m0
  k.2 <- 2^kbar
  g.m <- seq(0,k.2-1)

  for(i in 1:k.2){
    g <- 1

    for(j in 0:(kbar-1)){

      if(bitwAnd(g.m[i],2^j)!=0){
        g <- g*m1

      }else{
        g <- g*m0
      }
    }
    g.m[i] <- g;
  }

  g.m=matrix(sqrt(g.m),1,k.2)
  return(g.m)
}
