\name{print.MacroFactorModel}
\alias{print.MacroFactorModel}
\title{print MacrofactorModel object}
\usage{
  print.MacroFactorModel(fit.macro)
}
\arguments{
  \item{fit.macro}{fit object created by
  fitMacroeconomicFactorModel.}
}
\description{
  Generic function of print method for
  fitMacroeconomicFactorModel.
}
\examples{
# load data from the database
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
# fit the factor model with OLS
fit.macro <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                 variable.selection="all subsets")
print(fit.macro)
}
\author{
  Eric Zivot and Yi-An Chen.
}

