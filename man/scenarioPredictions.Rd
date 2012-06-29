\name{scenarioPredictions}
\alias{scenarioPredictions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Class performance prediction from user-defined scenarios
}
\description{
Class performance prediction from user-defined scenarios
}
\usage{
scenarioPredictions(fit.list, scenarioData, Betas, zero.alpha = TRUE, userBetas = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit.list}{
A list containing lm objects for classes with sufficient history. May not
have the same number of classes as in the matrix Betas.
}
  \item{scenarioData}{
matrix containing factor performance for user-defined scenarios.
}
  \item{Betas}{
matrix containing factor betas for all classes. May not have the same
 number of classes as in the list object fit.list.
}
  \item{zero.alpha}{
logical flag indicating if intercept should be subtracted from predictions.
}
  \item{userBetas}{
If userBetas=\code{FALSE}, then
forecast confidence intervals are included; otherwise, only forecasts
are included.
}
}

\value{
matrix containing scenario predictions. If userBetas=\code{FALSE}, then
forecast confidence intervals are included; otherwise, only forecasts
are included. No confidence intervals are computed for classes with
proxy betas
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
# Compute scenario prediction using our factor data. 
fit.list <- fit$asset.fit
# make up some scenario for factors
scenarioData <- matrix(rnorm(15,sd=0.01),5,3) 
colnames(scenarioData)= colnames(factors)
rownames(scenarioData)=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5")
pred <- scenarioPredictions(fit.list,scenarioData,fit$beta.mat,zero.alpha = TRUE,userBetas = TRUE)
head(pred)
}
