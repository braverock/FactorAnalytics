\name{fitFundamentalFactorModel}
\alias{fitFundamentalFactorModel}
\title{fit fundamental factor model by classic OLS or Robust regression technique}
\usage{
  fitFundamentalFactorModel(data, exposure.names, datevar,
    returnsvar, assetvar, wls = TRUE,
    regression = "classic", covariance = "classic",
    full.resid.cov = TRUE, robust.scale = FALSE)
}
\arguments{
  \item{data}{data.frame, data must have \emph{assetvar},
  \emph{returnvar}, \emph{datevar} , and exposure.names.
  Generally, data is panel data setup, so it needs firm
  variabales and time variables.}

  \item{exposure.names}{a character vector of exposure
  names for the factor model}

  \item{wls}{logical flag, TRUE for weighted least squares,
  FALSE for ordinary least squares}

  \item{regression}{A character string, "robust" for
  regression via lmRob, "classic" for regression with lm()}

  \item{covariance}{A character string, "robust" for
  covariance matrix computed with covRob(), "classic" for
  covariance matrix with covClassic() in robust package.}

  \item{full.resid.cov}{logical flag, TRUE for full
  residual covariance matrix calculation, FALSE for
  diagonal residual covarinace matrix}

  \item{robust.scale}{logical flag, TRUE for exposure
  scaling via robust scale and location, FALSE for scaling
  via mean and sd}

  \item{returnsvar}{A character string giving the name of
  the return variable in the data.}

  \item{datevar}{A character string giving the name of the
  date variable in the data.}

  \item{assetvar}{A character string giving the name of the
  asset variable in the data.}
}
\value{
  an S3 object containing \itemize{ \item cov.returns A
  "list" object contains covariance information for asset
  returns, includes covariance, mean and eigenvalus. \item
  cov.factor Anobject of class "cov" or "covRob" which
  contains the covariance matrix of the factor returns
  (including intercept). \item cov.resids An object of
  class "cov" or "covRob" which contains the covariance
  matrix of the residuals, if "full.resid.cov" is TRUE.
  NULL if "full.resid.cov" is FALSE. \item resid.variance A
  vector of variances estimated from the OLS residuals for
  each asset. If "wls" is TRUE, these are the weights used
  in the weighted least squares regressions.  If "cov =
  robust" these values are computed with "scale.tau".
  Otherwise they are computed with "var". \item factor.rets
  A "xts" object containing the times series of estimated
  factor returns and intercepts. \item resids A "xts"
  object containing the time series of residuals for each
  asset. \item tstats A "xts" object containing the time
  series of t-statistics for each exposure. \item call
  function call }
}
\description{
  fit fundamental factor model or cross-sectional time
  series factor model by classic OLS or Robust regression
  technique.  Fundamental factor models use observable
  asset specific characteristics (fundamentals) like
  industry classification, market capitalization, style
  classification (value, growth) etc. to determine the
  common risk factors. The function creates the class
  "FundamentalFactorModel".
}
\details{
  The original function was designed by Doug Martin and
  originally implemented in S-PLUS by a number of UW Ph.D.
  students:Christopher Green, Eric Aldrich, and Yindeng
  Jiang. Guy Yullen re-implemented the function in R and
  requires the following additional R libraries: zoo time
  series library, robust Insightful robust library ported
  to R and robustbase Basic robust statistics package for R
}
\examples{
\dontrun{
# BARRA type factor model
data(stock)
# there are 447 assets
exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP")
ttest.fit <- fitFundamentalFactorModel(data=data,exposure.names=exposure.names,
                                       datevar = "DATE", returnsvar = "RETURN",
                                       assetvar = "TICKER", wls = TRUE,
                                       regression = "classic",
                                       covariance = "classic", full.resid.cov = TRUE,
                                       robust.scale = TRUE)

names(test.fit)
test.fit$cov.returns
test.fit$cov.resids
names(test.fit$cov.factor)
test.fit$cov.factor$cov
test.fit$factor
test.fit$resid.variance
test.fit$resids
test.fit$tstats
test.fit$call

# BARRA type Industry Factor Model
exposure.names <- c("GICS.SECTOR")
# the rest keep the same
test.fit2 <- fitFundamentalFactorModel(data=data,exposure.names=exposure.names,
                                       datevar = "DATE", returnsvar = "RETURN",
                                       assetvar = "TICKER", wls = TRUE,
                                       regression = "classic",
                                       covariance = "classic", full.resid.cov = TRUE,
                                       robust.scale = TRUE)

names(test.fit2)
test.fit2$cov.returns
test.fit2$cov.resids
names(test.fit2$cov.factor)
test.fit2$cov.factor$cov
test.fit2$factor
test.fit2$resid.variance
test.fit2$resids
test.fit2$tstats
test.fit2$call



}
}
\author{
  Guy Yullen and Yi-An Chen
}

