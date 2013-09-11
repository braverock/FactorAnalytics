\name{print.FM.attribution}
\alias{print.FM.attribution}
\title{Print FM.attribution object.}
\usage{
  \method{print}{FM.attribution} (fm.attr)
}
\arguments{
  \item{fm.attr}{FM.attribution object created by
  \code{factorModelPerformanceAttribution}.}
}
\description{
  Generic function of print method for
  \code{factorModelPerformanceAttribution}.
}
\examples{
\dontrun{
# load data from the database
 data(managers.df)
 # fit the factor model with OLS
 fit.ts <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                               factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                               data=managers.df,fit.method="OLS")

  fm.attr <- factorModelPerformanceAttribution(fit.ts)
  print(fm.attr)
  }
}
\author{
  Yi-An Chen.
}

