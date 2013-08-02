\name{print.StatFactorModel}
\alias{print.StatFactorModel}
\title{print StatFactorModel object}
\usage{
  \method{print}{StatFactorModel} (x,
    digits = max(3, .Options$digits - 3), ...)
}
\arguments{
  \item{x}{fit object created by
  fitStatisticalFactorModel.}

  \item{digits}{integer indicating the number of decimal
  places. Default is 3.}

  \item{...}{Other arguments for print methods.}
}
\description{
  Generic function of print method for fitStatFactorModel.
}
\examples{
# load data for fitStatisticalFactorModel.r
# data from finmetric berndt.dat and folio.dat

data(stat.fm.data)
# pca
sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat,k=10)
print(sfm.pca.fit)
}
\author{
  Eric Zivot and Yi-An Chen.
}

