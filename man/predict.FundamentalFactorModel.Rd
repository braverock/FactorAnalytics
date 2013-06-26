\name{predict.FundamentalFactorModel}
\alias{predict.FundamentalFactorModel}
\title{predict method for FundamentalFactorModel object}
\usage{
  predict.FundamentalFactorModel(fit, newdata,
    new.assetvar, new.datevar)
}
\arguments{
  \item{fit}{"FundamentalFactorModel" object}

  \item{newdata}{An optional data frame in which to look
  for variables with which to predict.  If omitted, the
  fitted values are used.}

  \item{new.assetvar}{specify new asset variable in newdata
  if newdata is provided.}

  \item{new.datevar}{speficy new date variable in newdata
  if newdata is provided.}
}
\description{
  Generic function of predict method for
  fitFundamentalFactorModel.
}
\details{
  newdata must be data.frame and contians date variable,
  asset variable and exact exposures names that are used in
  fit object by \code{fitFundamentalFactorModel}
}
\author{
  Yi-An Chen
}

