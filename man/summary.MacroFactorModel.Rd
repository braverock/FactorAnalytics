\name{summary.MacroFactorModel}
\alias{summary.MacroFactorModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
summary MacrofactorModel object.
}
\description{
Generic function of summary method for fitMacroeconomicFactorModel. 
}
\usage{
\method{summary}{MacroFactorModel}(fit.macro)
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
summary(fit.macro)

}