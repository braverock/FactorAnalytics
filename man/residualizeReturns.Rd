% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitFfmDT.R
\name{residualizeReturns}
\alias{residualizeReturns}
\title{residualizeReturns}
\usage{
residualizeReturns(specObj, benchmark, rfRate, isBenchExcess = F)
}
\arguments{
\item{specObj}{specObj is a ffmSpec object,}

\item{benchmark}{we might need market returns}

\item{rfRate}{risk free rate}

\item{isBenchExcess}{toggle to select whether to calculate excess returns}
}
\description{
#' function to Residualize the returns via regressions
}
\details{
this function operates on the data inside the specObj and residualizes
the returns to create residual return using regressions of returns on a
benchmark.
}
\seealso{
\code{\link{specFfm}} for information on the definition of the specFfm object.
}
