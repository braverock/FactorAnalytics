\name{TreasuryYields}
\alias{TreasuryYields}
\alias{tr.yields}
\docType{data}
\title{
Treasury yields at different maturities
}
\description{
The following is adapted from chapter 17 of Ruppert (2010).

The data object contains yields on Treasury bonds at 11 maturities, T = 1, 3, and 6 months and 1, 2, 3, 5, 7, 10, 20, and 30 years. Daily yields were taken from a U.S. Treasury website for the time period January 2, 1990, to October 31, 2008.

Daily yields were missing from some values of T because, for example to quote the website, "Treasury discontinued the 20-year constant maturity series at the
end of calendar year 1986 and reinstated that series on October 1, 1993." Dif-
ferencing may cause a few additional days to have missing values.
}
\usage{data(TreasuryYields)}
\format{
xts time series object
\describe{
    \item{\code{tr.yields}}{Jan-02-1990 through Oct-31-2008}
  }
}
\source{
SDAFE author's website: \url{http://people.orie.cornell.edu/davidr/SDAFE/index.html}
}
\references{
Ruppert, D. (2010). Statistics and data analysis for financial engineering. Springer.
}
\examples{
data(TreasuryYields)
# preview the data
head(tr.yields)
}
\keyword{datasets}
\keyword{ts}

