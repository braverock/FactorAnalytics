\name{CommonFactors}
\alias{CommonFactors}
\alias{factors}
\alias{factors.Q}
\docType{data}
\title{
Factor set of several commonly used factors
}
\description{
10 monthly and quarterly common factors xts data from 1997-01-31 to 2013-07-31. 
\itemize{
\item SP500 is SP500 returns from FRED,
\item GS10TR US Treasury 10y yields total returns from the yeild of the 10 year constant maturity from FRED,
\item USD.Index Trade Weighted U.S. Dollar Index: Major Currencies - TWEXMMTH. from FRED
\item Term.Spread Yield spread of Merrill Lynch High-Yield Corporate Master II Index minus 10-year Treasury. from FRED
\item TED.Spread 3-Month Treasury Bill: Secondary Market Rate(TB3MS) - 3-Month Eurodollar Deposit Rate (London) (MED)3. from FRED.
\item DJUBSTR DJUBS Commodities index.
\item dVIX the first difference of the end-of-month value of the CBOE Volatility Index (VIX).
\item OILPRICE ""OILPRICE" from FRED.
\item TB3MS 3-Month Treasury Bill: Secondary Market Rate(TB3MS) from FRED
}

}
\usage{data(CommonFactors)}
\format{
  A data frame with 0 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\source{
\itemize{
\item FRED
\item http://www.djindexes.com/mdsidx/downloads/xlspages/ubsci_public/DJUBS_full_hist.xls
\item http://www.cboe.com/publish/ScheduledTask/MktData/datahouse/vixarchive.xls
}
}



