
# Linear factor model fitting for asset returns

The factorAnalytics package contains fitting and analysis methods for the three main types of factor models used in conjunction with portfolio construction, optimization and risk management, namely fundamental factor models, time series factor models and statistical factor models. The purpose of this project is to add key improvements to the package that will make it its basic features and capabilities close to those of commercial portfolio optimization and risk management products.

------------
Fundamental Factor Scores from S&P Global Market Intelligence
------------
S&P Global Market Intelligence has kindly provided firm fundamentals data
referred to as “scores” or “alpha factors” for educational use in the open source factorAnalytics
R package. The data is contained in the R data frame object “factorDataSPGMI”
consisting of the following cross-section of scores for approximately 300 stocks from 1990 to
2015: AccrualRatioCF, AnnVol12M, Beta60M, BP, Chg1YEPS, DivP, EBITDAEV, EP, EQ-style,
LogMktCap, PM12M1M, ROE. This data greatly facilitates the educational value to users of the
fundamental factor model in factorAnalytics. The package developers wish to thank S&P Global
Market Intelligence for contributing this data to the factorAnalytics package.

------------
Installation
------------

To get started, you can install the package from github using `devtools`.

``` r
library(devtools)
install_github("braverock/factorAnalytics")
```

------------

R/Finance 2017, Chicago
------------

[R Script](https://www.dropbox.com/s/jv809g196iyqo0k/FFM%20Talk%20Rcode%20R-finance2017.R?dl=0) and [slides](https://www.dropbox.com/s/gh4y8a6e9bcxwnv/ffmTalk%20RinFinance%202017.pdf?dl=0) used in Prof. Douglas Martin's "Fundamental Factor Models in FactorAnalytics" Pre-Conference Seminar.

------------

Boston useR Group 2017
------------

Click [here](https://www.dropbox.com/s/ibisg1y3yutej4m/cfrm%20fundamental%20facmods.pdf?dl=0) for the background slide deck for the Boston useR group talk by Prof. Doug Martin.