\name{CommonFactors}
\alias{CommonFactors}
\alias{factors.M}
\alias{factors.Q}
\docType{data}
\title{
Factor set of several commonly used factors
}
\description{
Collection of common factors as both monthly and quarterly time series
\itemize{
\item SP500: S&P 500 composite index returns. (Yahoo)
\item GS10TR: US Treasury 10y yields total returns from the yeild of the 10 year constant maturity. (FRED)
\item USD.Index: Trade Weighted U.S. Dollar Index: Major Currencies - TWEXMMTH. (FRED)
\item Term.Spread: Yield spread of Merrill Lynch High-Yield Corporate Master II Index minus 10-year Treasury. (FRED)
\item TED.Spread: 3-Month Treasury Bill: Secondary Market Rate(TB3MS) - 3-Month Eurodollar Deposit Rate (MED3). (FRED)
\item dVIX: First difference of the end-of-month value of the CBOE Volatility Index (VIX). (Yahoo)
\item OILPRICE: Monthly returns of spot price of West Texas Intermediate. (FRED)
\item TB3MS: 3-Month Treasury Bill Secondary Market Rate (TB3MS). (FRED)
}

}
\usage{data(CommonFactors)}
\format{
  xts time series object
  \describe{
    \item{\code{factors.M}}{Jan-1997 through May-2014}
    \item{\code{factors.Q}}{Q1-1997 through Q1-2014}
  }
}
\source{
\itemize{
\item Federal Reserve Economic Data (FRED):  \url{http://research.stlouisfed.org/fred2/}
\item Yahoo Finance: \url{http://finance.yahoo.com/}
}
}



