\name{fitTimeseriesFactorModel}
\alias{fitTimeseriesFactorModel}
\title{Fit time series factor model by time series regression techniques.}
\usage{
  fitTimeseriesFactorModel(assets.names, factors.names,
    data = data, num.factor.subset = 1,
    fit.method = c("OLS", "DLS", "Robust"),
    variable.selection = "none", decay.factor = 0.95,
    nvmax = 8, force.in = NULL,
    subsets.method = c("exhaustive", "backward", "forward", "seqrep"),
    lars.criteria = "Cp")
}
\arguments{
  \item{assets.names}{names of assets returns.}

  \item{factors.names}{names of factors returns.}

  \item{num.factor.subset}{scalar. Number of factors
  selected by all subsets.}

  \item{data}{a vector, matrix, data.frame, xts, timeSeries
  or zoo object with asset returns and factors retunrs
  rownames}

  \item{fit.method}{"OLS" is ordinary least squares method,
  "DLS" is discounted least squares method. Discounted
  least squares (DLS) estimation is weighted least squares
  estimation with exponentially declining weights that sum
  to unity. "Robust"}

  \item{variable.selection}{"none" will not activate
  variables sellection. Default is "none". "stepwise" is
  traditional forward/backward #' stepwise OLS regression,
  starting from the initial set of factors, that adds
  factors only if the regression fit as measured by the
  Bayesian Information Criteria (BIC) or Akaike Information
  Criteria (AIC) can be done using the R function step()
  from the stats package. If "Robust" is chosen, the
  function step.lmRob in Robust package will be used. "all
  subsets" is Traditional all subsets regression can be
  done using the R function regsubsets() from the package
  leaps. "lar" , "lasso" is based on package "lars", linear
  angle regression. If "lar" or "lasso" is chose.
  fit.method will be ignored.}

  \item{decay.factor}{for DLS. Default is 0.95.}

  \item{nvmax}{control option for all subsets. maximum size
  of subsets to examine}

  \item{force.in}{control option for all subsets. The
  factors that should be in all models.}

  \item{subsets.method}{control option for all subsets. se
  exhaustive search, forward selection, backward selection
  or sequential replacement to search.}

  \item{lars.criteria}{either choose minimum "Cp": unbiased
  estimator of the true rist or "cv" 10 folds
  cross-validation. Default is "Cp". See detail.}
}
\value{
  an S3 object containing \itemize{ \item{asset.fit}{Fit
  objects for each asset. This is the class "lm" for each
  object.} \item{alpha}{N x 1 Vector of estimated alphas.}
  \item{beta}{N x K Matrix of estimated betas.} \item{r2}{N
  x 1 Vector of R-square values.} \item{resid.variance}{N x
  1 Vector of residual variances.} \item{call}{function
  call.} }
}
\description{
  Fit time series factor model by time series regression
  techniques. It creates the class of
  "TimeSeriesFactorModel".
}
\details{
  If \code{Robust} is chosen, there is no subsets but all
  factors will be used.  Cp is defined in
  http://www-stat.stanford.edu/~hastie/Papers/LARS/LeastAngle_2002.pdf.
  p17.
}
\examples{
\dontrun{
# load data from the database
data(managers.df)
fit <- fitTimeseriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                                factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                                data=managers.df,fit.method="OLS")
# summary of HAM1
summary(fit$asset.fit$HAM1)
# plot actual vs. fitted over time for HAM1
# use chart.TimeSeries() function from PerformanceAnalytics package
dataToPlot = cbind(fitted(fit$asset.fit$HAM1), na.omit(managers.df$HAM1))
colnames(dataToPlot) = c("Fitted","Actual")
chart.TimeSeries(dataToPlot, main="FM fit for HAM1",
                 colorset=c("black","blue"), legend.loc="bottomleft")
 }
}
\author{
  Eric Zivot and Yi-An Chen.
}
\references{
  1. Efron, Hastie, Johnstone and Tibshirani (2002) "Least
  Angle Regression" (with discussion) Annals of Statistics;
  see also
  http://www-stat.stanford.edu/~hastie/Papers/LARS/LeastAngle_2002.pdf.
  2. Hastie, Tibshirani and Friedman (2008) Elements of
  Statistical Learning 2nd edition, Springer, NY.
}

