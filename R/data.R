#' Data from "How to Forecast Long-Run Volatility Paper"
#'
#' Data from "How to Forecast Long-Run Volatility:Regime Switching and the
#' Estimation of Multifractal Processes".
#' Daily exchange rate data for the deutsche mark (dm), Japanese yen (ja),
#' British pound (uk), and Canadian dollar (ca), all against the U.S. dollar.
#' The data consist of daily prices (buying rates for wire transfers at 12:00 P.M. Eastern time)
#'  reported at noon by the Federal Reserve Bank of New York .
#'
#' @docType data
#'
#' @usage data(calvet2004data)
#' @name calvet2004data
#' @format An object of class \code{"data.frame"}.
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
#' \donttest{summary(caret)}
NULL

#' Data from "Volatility comovement: a multifrequency approach" Paper.
#'
#' Data from "Volatility comovement: a multifrequency approach".
#' Daily exchange rate data for the Japanese yen (ja),
#' British pound (uk), and Canadian dollar (ca), all against the U.S. dollar.
#' The data consist of daily prices (buying rates for wire transfers at 12:00 P.M. Eastern time)
#'  reported at noon by the Federal Reserve Bank of New York .
#'
#' @docType data
#'
#' @usage data(calvet2006returns)
#' @name calvet2006returns
#'
#' @format A data frame with 7635 rows and 4 variables:
#' \describe{
#'   \item{Date}{Daily date}
#'   \item{CanRet}{Log returns of Canada / U.S. Foreign Exchange Rate, Canadian Dollars to One U.S. Dollar, Daily}
#'   \item{JARet}{Log returns of Japan / U.S. Foreign Exchange Rate, Japanese Yen to One U.S. Dollar, Daily}
#'   \item{UKRet}{Log returns of U.K. / U.S. Foreign Exchange Rate, U.S. Dollars to One British Pound, Daily}
#'   }
#'
#' @keywords datasets
#'
#' @references Calvet, L.E & Fisher, J.A (2006) Volatility comovement: a multifrequency approach
#' (\href{http://www.sciencedirect.com/science/article/pii/S0304-4076(05)00010-2}{PubMed})
#'
#' @source \href{https://fred.stlouisfed.org}{Federal Reserve Economic Data}
#'
#' @examples
#' data(calvet2006returns)
#' caret <- as.matrix(calvet2006returns$CanRet)
#' \donttest{summary(caret)}
NULL
