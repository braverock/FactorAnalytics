\name{fitFundamentalFactorModel}
\alias{fitFundamentalFactorModel}
\title{fit fundamental factor model by classic OLS or Robust regression technique}
\usage{
  fitFundamentalFactorModel(fulldata, timedates, exposures,
    assets, wls = FALSE, regression = "classic",
    covariance = "classic", full.resid.cov = TRUE,
    robust.scale = FALSE, datevar = "DATE",
    assetvar = "PERMNO", returnsvar = "RETURN",
    tickersvar = "TICKER.x")
}
\arguments{
  \item{fulldata}{data.frame, fulldata contains returns,
  dates, and exposures which is stacked by dates.}

  \item{timedates}{a vector of Dates specifying the date
  range for the model fitting}

  \item{exposures}{a character vector of exposure names for
  the factor model}

  \item{assets}{a list of PERMNOs to be used for the factor
  model}

  \item{wls}{logical flag, TRUE for weighted least squares,
  FALSE for ordinary least squares}

  \item{regression}{A character string, "robust" for
  regression via lmRob, "classic" for regression with lm()}

  \item{covariance}{A character string, "robust" for
  covariance matrix computed with covRob(), "classic" for
  covariance matrix with ccov()}

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

  \item{tickersvar}{A character string giving the name of
  the ticker variable in the data.}
}
\value{
  an S3 object containing
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
assets = unique(fulldata[,"PERMNO"])
timedates = as.Date(unique(fulldata[,"DATE"]))
exposures <- exposures.names <- c("BOOK2MARKET", "LOG.MARKETCAP")
test.fit <- fitFundamentalFactorModel(fulldata=fulldata, timedates=timedates, exposures=exposures,covariance="classic", assets=assets,full.resid.cov=TRUE,
                                      regression="classic",wls=TRUE)
names(test.fit)
test.fit$cov.returns
test.fit$cov.facrets
test.fit$facrets

# BARRA type Industry Factor Model
exposures <- exposures.names <- c("GICS.SECTOR")
# the rest keep the same
test.fit <- fitFundamentalFactorModel(fulldata=fulldata, timedates=timedates, exposures=exposures,
                                      covariance="classic", assets=assets,full.resid.cov=TRUE,
                                      regression="classic",wls=TRUE)
}
}
\author{
  Guy Yullen and Yi-An Chen
}

