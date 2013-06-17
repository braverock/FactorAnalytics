\name{fitMacroeconomicFactorModel}
\alias{fitMacroeconomicFactorModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit macroeconomic factor model by time series regression techniques.
}
\description{
Fit macroeconomic factor model by time series regression techniques. It creates the class of "MacroFactorModel".
}
\usage{
fitMacroeconomicFactorModel(ret.assets, factors, factor.set = 2, fit.method =
                            c("OLS", "DLS", "Robust"), variable.selection =
                            c("stepwise", "all subsets", "lar", "lasso"),
                            decay.factor = 0.95, nvmax = 8, force.in = NULL,
                            subsets.method = c("exhaustive", "backward",
                           "forward", "seqrep"), lars.criteria = c("Cp", "cv"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ret.assets}{
N x T Numerical returns data, univariate or multivariate, where N is the number of assets return and T is the length of time period. data has to be saved as class "data.frame" so that lm function can be used and must have column names.
}
  \item{factors}{
K x T Numerical factors data, where K is the number of factors and T is the length of the time period. Data has to be saved as class "data.frame" so that lm function can be used and must have column names.
}
  \item{factor.set}{
scalar, number of factors
}
\item{fit.method}{
"OLS" is ordinary least squares method, "DLS" is discounted least squares method. Discounted least squares (DLS) estimation is weighted least squares estimation with exponentially declining weights that sum to
unity. "Robust"
}
\item{variable.selection}{
"stepwise" is traditional forward/backward stepwise OLS regression, starting from the initial 
set of factors, that adds factors only if the regression fit as measured by the Bayesian 
Information Criteria (BIC) or Akaike Information Criteria (AIC) can be done using the R 
function step() from the stats package. If \code{Robust} is chosen, the function step.lmRob 
in Robust package will be used. "all subsets" is Traditional all subsets regression 
can be done using the R function regsubsets() from the package leaps. "lar" , "lasso" is based on package "lars", linear angle regression.
}
\item{decay.factor}{
for DLS. Default is 0.95.
}
\item{nvmax}{
control option for all subsets. maximum size of subsets to examine
}             
\item{force.in}{
control option for all subsets. The factors that should be in all models.
}
\item{subsets.method}{
control option for all subsets. se exhaustive search, forward selection, backward selection 
or sequential replacement to search.
}
\item{lars.criteria}{
either choose minimum "Cp": unbiased estimator of the true rist or "cv"
10 folds cross-validation. See detail.
}        
}
\details{
If \code{Robust} is chosen, there is no subsets but all factors will be used. 
Cp is defined in http://www-stat.stanford.edu/~hastie/Papers/LARS/LeastAngle_2002.pdf. p17.
}
\value{
  \item{asset.fit}{
  Fit objects for each asset. This is the class "lm" for each object.
  }
  \item{alpha.vec}{
  N x 1 Vector of estimated alphas.
  }
  \item{beta.mat}{
  N x K Matrix of estimated betas
  }
  \item{r2.vec}{
  N x 1 Vector of R-square values.
  }
  \item{residVars.vec}{
  N x 1 Vector of residual variances.
  }
  \item{call}{
  function call.
  }
  \item{ret.assets}{
  Assets returns of input data.
  }
  \item{factors}{
  Factors of input data.
  }
  \item{variable.selection}{
  variables selected by the user.
  }
}
\references{
1. Efron, Hastie, Johnstone and Tibshirani (2002) "Least Angle Regression" (with discussion) Annals of Statistics; see also http://www-stat.stanford.edu/~hastie/Papers/LARS/LeastAngle_2002.pdf. 
2. Hastie, Tibshirani and Friedman (2008) Elements of Statistical Learning 2nd edition, Springer, NY.
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
fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                 variable.selection="all subsets")
# summary of HAM1 
summary(fit$asset.fit$HAM1)
# plot actual vs. fitted over time for HAM1
# use chart.TimeSeries() function from PerformanceAnalytics package
dataToPlot = cbind(fitted(fit$asset.fit$HAM1), na.omit(managers.df$HAM1))
colnames(dataToPlot) = c("Fitted","Actual")
chart.TimeSeries(dataToPlot, main="FM fit for HAM1",
                 colorset=c("black","blue"), legend.loc="bottomleft")
}

