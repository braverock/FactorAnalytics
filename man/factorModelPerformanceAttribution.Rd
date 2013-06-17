\name{factorModelPerformanceAttribution}
\alias{factorModelPerformanceAttribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute BARRA-type performance attribution
}
\description{
Decompose total returns or active returns into returns attributed to factors and specific returns. Class of FM.attribution is generated and generic function \code{plot} and \code{summary} can be used.   
}
\usage{
factorModelPerformanceAttribution(fit,benchmark=NULL,...)
}

\arguments{
  \item{fit}{
Class of "MacroFactorModel", "FundamentalFactorModel" or "statFactorModel".
}
  \item{benchmark}{
a zoo, vector or data.frame provides benchmark time series returns.
}
 \item{...}{
 Other controled variables for fit methods.  
 }
  
}
\details{
total returns can be decomposed into returns attributed to factors and specific returns.
\eqn{R_t = \sum_j e_{jt} * f_{jt} + u_t},t=1..T,\eqn{e_{jt}} is exposure to factor j and \eqn{f_{jt}} is factor j. The returns attributed to factor j is \eqn{e_{jt} * f_{jt}} and portfolio specific returns is \eqn{u_t}  
}
\value{
\item{cum.ret.attr.f}{
N X J matrix of cumulative return attributed to factors. 
}
\item{cum.spec.ret}{
1 x N vector of cumulative specific returns.
}
\item{attr.list}{
list of time series of attributed returns for every portfolio.
}
}
\references{
Grinold,R and Kahn R, \emph{Active Portfolio Management}, McGraw-Hill.
}
\author{
Yi-An Chen.
}
\examples{
\dontrun{
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
fit.macro <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS", factor.set = 3,
                                         variable.selection="all subsets",decay.factor = 0.95)
# withoud benchmark
fm.attr <- factorModelPerformanceAttribution(fit.macro)

}
}
