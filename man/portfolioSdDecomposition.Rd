\name{portfolioSdDecomposition}
\alias{portfolioSdDecomposition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute portfolio sd (risk) decomposition by asset.
}
\description{
Compute portfolio sd (risk) decomposition by asset.
}
\usage{
portfolioSdDecomposition(w.vec, cov.assets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w.vec}{
    N x 1 vector of portfolio weights
}
  \item{cov.assets}{
    N x N asset covariance matrix
}
}

\value{
   \item{sd.p}{
   Scalar, portfolio standard deviation.
   }          
   \item{mcsd.p}{
    1 x N vector, marginal contributions to portfolio standard deviation.
}        
   \item{csd.p}{
   1 x N vector, contributions to portfolio standard deviation.
   }         
   \item{pcsd.p}{
   1 x N vector, percent contribution to portfolio standard deviation.
}
}
\references{
Qian, Hua and Sorensen (2007) Quantitative Equity Portfolio Management, chapter 3.
}
\author{
Eric Zivot and Yi-An Chen
}

\examples{
# load data from the database
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
# fit the factor model with OLS
fit <-fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                  variable.selection="all subsets", factor.set = 3)
# factor SD decomposition for HAM1
cov.factors = var(factors)
manager.names = colnames(managers.df[,(1:6)])
factor.names  = colnames(managers.df[,(7:9)])
# assuming equal weight vector
w.vec = rep(1/6,6)
# compute with sample covariance matrix (omit NA)
cov.sample = ccov(managers.df[,manager.names],na.action=na.omit)
port.sd.decomp.sample = portfolioSdDecomposition(w.vec, cov.sample$cov)
# show bar chart
barplot(port.sd.decomp.sample$pcsd.p,
        main="Fund Percent Contributions to Portfolio SD",
        ylab="Percent Contribution", legend.text=FALSE,
        col="blue")

# compute with factor model covariance matrix
returnCov.mat<- factorModelCovariance(fit$beta.mat,var(factors),fit$residVars.vec)
port.sd.decomp.fm = portfolioSdDecomposition(w.vec, returnCov.mat)                                                         

}

