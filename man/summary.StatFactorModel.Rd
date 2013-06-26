\name{summary.StatFactorModel}
\alias{summary.StatFactorModel}
\title{summary method for StatFactorModel object.}
\usage{
  summary.StatFactorModel(fit, ...)
}
\arguments{
  \item{fit}{"StatFactorModel" object created by
  fitStatisticalFactorModel.}

  \item{newdata}{An optional data frame in which to look
  for variables with which to predict. If omitted, the
  fitted values are used.}

  \item{...}{Any other arguments used in \code{summary.lm}}
}
\description{
  Generic function of summary method for
  fitStatisticalFactorModel. It utilizes function
  \code{summary.lm}.
}
\examples{
data(stat.fm.data)
.fit <- fitStatisticalFactorModel(sfm.dat,k=2,

summary(fit)
}
\author{
  Yi-An Chen. '
}

