\name{predict.StatFactorModel}
\alias{predict.StatFactorModel}
\title{predict method for StatFactorModel object.}
\usage{
  predict.StatFactorModel(fit, ...)
}
\arguments{
  \item{fit}{"StatFactorModel" object created by
  fitStatisticalFactorModel.}

  \item{...}{Any other arguments used in \code{predict.lm}.
  For example like newdata and fit.se.}
}
\description{
  Generic function of predict method for
  fitStatisticalFactorModel. It utilizes function
  \code{predict.lm}.
}
\examples{
data(stat.fm.data)
.fit <- fitStatisticalFactorModel(sfm.dat,k=2,

predict(fit)
}
\author{
  Yi-An Chen. '
}

