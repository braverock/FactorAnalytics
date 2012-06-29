\name{scenarioPredictionsPortfolio}
\alias{scenarioPredictionsPortfolio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Portfolio performance prediction from user-defined scenarios.
}
\description{
Portfolio performance prediction from user-defined scenarios.
}
\usage{
scenarioPredictionsPortfolio(scenarioData, factorData, beta.mat, cov.fm, w.vec,
                             level = 0.95, zero.alpha = TRUE, userBetas = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scenarioData}{
matrix containing factor performance for user-defined scenarios.
}
  \item{factorData}{
dataframe containing historical factor performance over estimation window.
}
  \item{beta.mat}{
k x 1 matrix containing portfolio factor betas.
}
  \item{w.vec}{
  n x 1 vector of portfolio weights
  }
  \item{cov.fm}{
n x n excess return covariance matrxi based on estimated factor model.
}
  \item{level}{
probablility level for confidence interval.
}
  \item{zero.alpha}{
logical flag indicating if intercept should be subtracted from predictions.
}
  \item{userBetas}{
logical. If userBetas=\code{FALSE}, then forecast confidence intervals are 
included; otherwise, only forecasts are included.
}
}

\value{
matrix containing scenario predictions. If userBetas=FALSE, then
forecast confidence intervals are included; otherwise, only forecasts
are included. No confidence intervals are computed for classes with
proxy betasm{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Eric Zivot and Yi-An Chen. 
}
\examples{
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
manager.names = colnames(managers.df[,(1:6)])
factor.names  = colnames(managers.df[,(7:9)])
w.vec = rep(1/6,6)
# fit the factor model with OLS
fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                   variable.selection = "all subsets",
                                   factor.set = 3)
# make up some scenario for factors
scenarioData <- matrix(rnorm(15,sd=0.01),5,3) 
colnames(scenarioData)= colnames(factors)
rownames(scenarioData)=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5")
factorData <- factors
beta.mat <- fit$beta.mat 
cov.fm <- factorModelCovariance(fit$beta.mat,var(factors),fit$residVars.vec)
# compute portfolio scenario prediction
scenarioPredictionsPortfolio(scenarioData,factorData,beta.mat,cov.fm,w.vec)
}
