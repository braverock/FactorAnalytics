% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assetDecomp.R
\name{assetDecomp}
\alias{assetDecomp}
\title{Decompose portfolio risk into individual asset contributions and provide tabular report}
\usage{
assetDecomp(object, weights = NULL, rm, p, type = c("np", "normal"), ...)
}
\arguments{
\item{object}{fit object of class \code{tsfm}, or \code{ffm}.}

\item{weights}{vector of weights of the assets in the portfolio. Default is NULL, in which case an 
equal weights will be used.}

\item{rm}{one of "Sd" (Standard Deviation) or "VaR" (Value at Risk) or "ES" (Expected Shortfall)}

\item{p}{tail probability for calculation. Default is 0.05.}

\item{type}{one of "np" (non-parametric) or "normal". Default is "np".}

\item{...}{other optional arguments}
}
\value{
Risk Decomposition report for every asset in the portfolio
}
\description{
Compute the asset contributions to Sd, VaR and ES of returns based on Euler's theorem
}
\examples{
# Fundamental Factor Model
data("factorDataSetDjia5Yrs")
data("wtsDjiaGmvLo")

# fit a fundamental factor model
exposure.vars <- c("P2B", "MKTCAP")
fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
                date.var="DATE", exposure.vars=exposure.vars, z.score = "crossSection")
                
#non-parametric 
weights = wtsDjiaGmvLo               
sd.decomp = assetDecomp(fit, weights, rm = "Sd")
VaR.decomp = assetDecomp(fit, weights, rm = "VaR", p = 0.05)
ES.decomp = assetDecomp(fit, weights, rm = "ES", p = 0.05)                 

#normal dist
VaR.decomp = assetDecomp(fit, weights, rm = "VaR", p = 0.05, type = "normal")
ES.decomp = assetDecomp(fit, weights, rm = "ES", p = 0.05, type = "normal") 
}
\references{
Epperlein and Smillie (2006) "Cracking VAR with Kernels" Risk.net
}
\seealso{
\code{\link{riskDecomp}}
for the Risk Decomposition function based on factors in the fitted model.
}
\author{
Avinash Acharya
}
