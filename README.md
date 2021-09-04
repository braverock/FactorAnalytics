
# Factor Analytics for asset return data

The FactorAnalytics project is an open source package containing fitting and analysis methods for the three main types of factor models commonly used in conjunction with portfolio construction, optimization and risk management. These include:
- fundamental factor models
- time series factor models
- statistical factor models

The purpose of this project is to provide basic features and capabilities close to those of commercial portfolio optimization and risk management products.

In addition, functions for related risk and performance attribution to estimate:
- volatility
- VaR
- ES
- factor-contributed vs idiosyncratic returns
- factor model Monte Carlo
- multiple imputation methods for simulating returns and backfilling unequal histories
- functions to create tabular displays of risk and performance reports


## Fundamental Factor Scores from S&P Global Market Intelligence
 
S&P Global Market Intelligence has kindly provided firm fundamentals data
referred to as “scores” or “alpha factors” for educational use in the open source FactorAnalytics R package. The data is contained in the R data frame object “factorDataSPGMI” consisting of the following cross-section of scores for approximately 300 stocks from 1990-2015: `AccrualRatioCF`, `AnnVol12M`, `Beta60M`, `BP`, `Chg1YEPS`, `DivP`, `EBITDAEV`, `EP`, `EQ-style`,
`LogMktCap`, `PM12M1M`, `ROE`. This data greatly facilitates the educational value to users of the fundamental factor model in FactorAnalytics. The package developers wish to thank S&P Global Market Intelligence for kindly contributing this data, which will improving learning outcomes for all.


## Installing from Source

At present, this is the preferred method of installation. To do so, your git installation will first need to support git large file storage (LFS).  

Instruction for installing are LFS are here: https://docs.github.com/en/github/managing-large-files/versioning-large-files/installing-git-large-file-storage

Plenty of documentation exists for cloning github repos and then building packages locally. A simple search specifying your IDE and OS should do.

Note a bug in `remotes`/`devtools` prevents the package from being loaded using `install_github("braverock/FactorAnalytics")`. Please see [r-lib/remotes/issues/637](https://github.com/r-lib/remotes/issues/637).


## Presentations and vignettes on FactorAnalytics

### Fundamental Factor Models vignette

From the first paragraph

_The overarching long-term goal of the fundamental factor model (Ffm) development in the FactorAnalytics packages is to replicate a large proportion of the non-proprietary models and model fitting and analysis methodology that is contained in commercial portfolio construction and risk management products such as MSCI Barra, Axioma, Northfield, etc. Furthermore our goals include the implementation cutting edge methods to support portfolio construction and risk management that are not much available in commercial products, such as global optimization, unequal histories and other missing data handling, highly robust covariance matrix estimators and their application to multivariate exposures and returns outliers, optimal bias robust regression, factor model Monte Carlo, new methods for handling serial correlation that improve upon traditional HAC methods, etc._

https://github.com/braverock/FactorAnalytics/blob/master/vignettes/Fundamental-Factor-Models-FactorAnalytics.pdf


### R/Finance 2017, Chicago

[R Script](https://www.dropbox.com/s/jv809g196iyqo0k/FFM%20Talk%20Rcode%20R-finance2017.R?dl=0) and [slides](https://www.dropbox.com/s/gh4y8a6e9bcxwnv/ffmTalk%20RinFinance%202017.pdf?dl=0) used in Prof. Douglas Martin's "Fundamental Factor Models in FactorAnalytics" Pre-Conference Seminar.

### Boston useR Group 2017
 
Click [here](https://www.dropbox.com/s/ibisg1y3yutej4m/cfrm%20fundamental%20facmods.pdf?dl=0) for the background slide deck for the Boston useR group talk by Prof. Doug Martin.


