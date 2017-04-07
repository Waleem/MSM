Bmsm_decompose <- function(smoothed.p, m01, m02, kbar) {


    marginals <- Bmsm_marginals(smoothed.p, m01, m02, kbar)

    palpha <- marginals$palpha
    pbeta <- marginals$pbeta

    em1 <- m01 * palpha + (2 - m01) * (1 - palpha)
    em2 <- m02 * pbeta + (2 - m01) * (1 - pbeta)

    em <- list(em1=em1, em2=em2)
}