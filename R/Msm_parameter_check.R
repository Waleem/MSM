#' Performs boundary checks on Msm model parameters.
#'
#' Performs boundary checks on Msm model parameter values.
#'
#' @param dat is a column matrix/ dataframe of returns.
#' @param kbar is the number of frequency components in the \code{\link{Msm}}(k) model.
#' @param x0 is the initial parameter values passed to Msm.
#'
#' @return a list consisting of:
#' \item{dat}{is a column matrix of returns}
#' \item{kbar}{number of frequency components}
#' \item{start.value}{is the initial parameter values passed to Msm}
#' \item{lb}{lower bound on parameters}
#' \item{ub}{lower bound on parameters}

#'
#' @examples
#' data("calvet2004data")
#' ret <- na.omit(as.matrix(dat$caret))*100
#' Msm_parameter_check (ret,2,c(1.5,10,.1,4))
#'
#' @export
Msm_parameter_check <- function(dat,kbar,x0){

  # Check for valid dat entry:
  if(!is.matrix(dat)){
    dat <- as.matrix(dat)
  }
  if(ncol(dat) > 1){
    dat <- t(dat)
  }
  if(ncol(dat) > 1|nrow(dat) < 2|is.null(dat)) stop('dat must be a numeric vector.')

  # Check for valid parameter inputs:
  if(kbar < 1) stop('kbar (number of volatility clusters) must be a positive integer.')

  if(!is.null(x0)){

    if(length(x0) == 4){
      if (x0[1] < 1 || x0[1] > 1.99) stop('m0 must be between (1,1.99]')

      if (x0[2] < 1)  stop('b must be greater than 1')

      if (x0[3] < 0.0001 || x0[3] > 0.9999) stop('gamma_k be between [0,1]')

      if (x0[4] < 0.00001) stop('sigma must be a positive (non-zero) value')

    } else{
      stop('Initial values must be of length 4 in the form c(m0,b,gammak,sigma)')
    }


  } else {
    x0 <- c(1.5,2.5,0.9,sd(dat))
  }

  lb = c(1, 1, 0.0001, 0.0001)
  ub = c(1.9999, 50, 0.9999, 50)


  msm.check <- list(dat         = dat,
                    kbar        =kbar,
                    start.value = x0,
                    lb          = lb,
                    ub          = ub)
  return(msm.check)
}

