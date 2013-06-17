\name{print.MacroFactorModel}
\alias{print.MacroFactorModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
print MacrofactorModel object
}
\description{
Generic function of print method for fitMacroeconomicFactorModel.
}
\usage{
\method{print}{MacroFactorModel}(fit.macro)
}

\arguments{
  \item{fit.macro}{
fit object created by fitMacroeconomicFactorModel.
}
}

\author{
Eric Zivot and Yi-An Chen.
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