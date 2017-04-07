#' Data from "How to Forecast Long-Run Volatility Paper"
#'
#' Data from "How to Forecast Long-Run Volatility:Regime Switching and the 
#' Estimation of Multifractal Processes"
#' Daily exchange rate data for the deutsche mark (dm), Japanese yen (ja), 
#' British pound (uk), and Canadian dollar (ca), all against the U.S. dollar.
#' The data consist of daily prices (buying rates for wire transfers at 12:00 P.M. Eastern time)
#'  reported at noon by the Federal Reserve Bank of New York .
#'
#' @docType data
#'
#' @usage data(calvet2004data)
#'
#' @format An object of class \code{"data.frame"}; see  \code{\link{Msm}}.
#'
#' @keywords datasets
#'
#' @references Calvet, L.E & Fisher, J.A (2004) How to Forecast Long-Run Volatility:Regime Switching and the 
#' Estimation of Multifractal Processes
#' (\href{https://academic.oup.com/jfec/article-abstract/2/1/49/960710/How-to-Forecast-Long-Run-Volatility-Regime}{PubMed})
#'
#' @source \href{https://fred.stlouisfed.org}{Federal Reserve Economic Data}
#'
#' @examples
#' data(calvet2004data)
#' caret <- as.matrix(na.omit(calvet2004data$caret))
#' summary(caret)
#' \donttest{summary(caret)}
"calvet2004data"
