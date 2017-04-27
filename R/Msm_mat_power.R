#' Internal Routine Used By \code{\link{Msm}} Model.
#'
#' Calculates matrix exponential, i.e A*A*A...*A
#'
#' @param A is matrix.
#' @param power is the power of the matrix.
#'
#'
#' @return A^power.
#'
Msm_mat_power <- function(A,power){
  base = A
  out = diag(nrow(A))
  while(power > 1){
    if(power %% 2 == 1){
      out = out %*% base
    }
    base = base %*% base
    power  = power %/% 2
  }
  out %*% base
}
