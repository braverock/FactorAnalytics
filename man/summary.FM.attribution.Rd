\name{summary.FM.attribution}
\alias{summary.FM.attribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  summary FM.attribution object.
}
\description{
  Generic function of summary method for factorModelPerformanceAttribution. 
}
\usage{
  \method{summary}{FM.attribution}(fm.attr)
}

\arguments{
  \item{fm.attr}{
FM.attribution object created by factorModelPerformanceAttribution.
}
}



\author{
  Yi-An Chen.
}

\examples{
  \dontrun{
  fm.attr <- factorModelPerformanceAttribution(fit.macro)
  summary(fm.attr)
  }
  
}