\docType{data}
\name{managers.df}
\alias{managers.df}
\title{Hypothetical Alternative Asset Manager and Benchmark Data}
\description{ 
A data.frame version of the "managers" xts data object from the 
PerformanceAnalytics package. It contains columns of monthly returns for six hypothetical asset managers (HAM1 through HAM6), the EDHEC Long-Short Equity hedge fund index, the S\&P 500 total returns. Monthly returns for all series 
end in December 2006 and begin at different periods starting from January 1997.

Note that all the EDHEC indices are available in \code{\link{edhec}}.
}
\usage{data(managers)}

\examples{
data(managers.df)

# preview the data
head(managers.df)

# get colnames of HAM fund returns
asset.names <- colnames(managers.df[,1:6])
}
\keyword{datasets}

