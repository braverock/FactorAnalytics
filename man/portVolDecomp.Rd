% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/portVolDecomp.R
\name{portVolDecomp}
\alias{portVolDecomp}
\alias{portVolDecomp.tsfm}
\alias{portVolDecomp.ffm}
\title{Decompose portfolio variance risk into factor/residual risk}
\usage{
portVolDecomp(object, ...)

\method{portVolDecomp}{tsfm}(
  object,
  weights = NULL,
  factor.cov,
  use = "pairwise.complete.obs",
  ...
)

\method{portVolDecomp}{ffm}(object, weights = NULL, factor.cov, ...)
}
\arguments{
\item{object}{fit object of class \code{tsfm}, or \code{ffm}.}

\item{...}{optional arguments passed to \code{\link[stats]{cov}}.}

\item{weights}{a vector of weights of the assets in the portfolio.
Default is NULL, in which case an equal weights will be used.}

\item{factor.cov}{optional user specified factor covariance matrix with
named columns; defaults to the sample covariance matrix.}

\item{use}{an optional character string giving a method for computing
covariances in the presence of missing values. This must be (an
abbreviation of) one of the strings "everything", "all.obs",
"complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is
"pairwise.complete.obs".}
}
\value{
A vector containing: percent factor contribution to risk
portfolio volatility risk, factor volatility risk and
residual/specific volatility risk
}
\description{
Decompose portfolio variance risk into factor/residual risk
}
\examples{
# Time Series Factor Model example

\dontrun{
 # load data
data(managers, package = 'PerformanceAnalytics')

fit.macro <- fitTsfm(asset.names = colnames(managers[,(1:6)]),
                     factor.names = colnames(managers[,(7:9)]),
                     rf.name = colnames(managers[,10]),
                     data = managers)

decomp <- portVolDecomp(fit.macro)

decomp

# Fundamental Factor Model example

## First load CRSP and SPGMI data sets
data(stocksCRSP)
data(factorsSPGMI)
## merge by intersection variables
intersecting_vars <- intersect(names(stocksCRSP), names(factorsSPGMI))
   stocks_factors <- merge(stocksCRSP, factorsSPGMI, by = intersecting_vars)
## Remove observations with missing Sector/GICS
      NA_index <- is.na(stocks_factors$GICS) & is.na(stocks_factors$Sector)
stocks_factors <- stocks_factors[!NA_index]
## Setindex for faster processing
data.table::setindexv(stocks_factors, c("Date","TickerLast"))


# fit a fundamental factor model

exposure_vars = c("Sector", "AnnVol12M", "BP", "EP", "LogMktCap", "PM12M1M")

fit.cross <- fitFfm(data = stocks_factors,
                    asset.var = "TickerLast",
                    ret.var = "Return",
                    date.var = "Date",
                    exposure.vars = exposure_vars,
                    fit.method = "W-Rob",
                    z.score = "crossSection")

decomp <- portVolDecomp(fit.cross)

# get the factor contributions of risk
decomp
 }

}
\seealso{
\code{\link{fitTsfm}}, \code{\link{fitFfm}}
for the different factor model fitting functions.

\code{\link{portSdDecomp}} for portfolio factor model VaR decomposition.
\code{\link{portVaRDecomp}} for portfolio factor model VaR decomposition.
\code{\link{portEsDecomp}} for portfolio factor model ES decomposition.
}
\author{
Douglas Martin, Lingjie Yi
}
