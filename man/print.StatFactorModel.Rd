\name{print.StatFactorModel}
\alias{print.StatFactorModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
print StatFactorModel object
}
\description{
Generic function of print method for fitStatFactorModel.
}
\usage{
\method{print}{StatFactorModel}(fit.stat, digits = max(3, .Options$digits - 3), ...)
}
\arguments{
  \item{fit.stat}{
fit object created by fitMacroeconomicFactorModel.
}
\item{digits}{
maximum digits. Default is 3. 
}
\item{...}{
Other variables for print methods.
}
}
\author{
Eric Zivot and Yi-An Chen.
}

\examples{
# load data for fitStatisticalFactorModel.r
# data from finmetric berndt.dat and folio.dat

data(stat.fm.data)
# pca
sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat,k=10)
print(sfm.pca.fit)

}