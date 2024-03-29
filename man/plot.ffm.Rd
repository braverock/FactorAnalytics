% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.ffm.R
\name{plot.ffm}
\alias{plot.ffm}
\title{Plots from a fitted fundamental factor model}
\usage{
\method{plot}{ffm}(
  x,
  which = NULL,
  f.sub = 1:2,
  a.sub = 1:6,
  plot.single = FALSE,
  asset.name,
  asset.variable,
  colorset = c("royalblue", "dimgray", "olivedrab", "firebrick", "goldenrod",
    "mediumorchid", "deepskyblue", "chocolate", "darkslategray"),
  legend.loc = "topleft",
  las = 1,
  lwd = 2,
  maxlag = 15,
  ...
)
}
\arguments{
\item{x}{an object of class \code{ffm} produced by \code{fitFfm}.}

\item{which}{a number to indicate the type of plot. If multiple plots are
required, specify a subset from 1:12 for group plots and 1:13 for individual
plots. If \code{which=NULL} (default), the following menu appears: \cr \cr
For plots of a group of assets: \cr
1 = Distribution of factor returns, \cr
2 = Factor exposures from the last period, \cr
3 = Actual and Fitted asset returns, \cr
4 = Time-series of R-squared values, \cr
5 = Residual variance across assets, x \cr
6 = Scatterplot matrix of residuals, with histograms, density overlays, correlations and significance stars, \cr
7 = Factor Model Residual Correlation \cr
8 = Factor Model Return Correlation,\cr
9 = Factor Contribution to SD,\cr
10 = Factor Contribution to ES,\cr
11 = Factor Contribution to VaR, \cr
12 = Time series of factor returns, \cr \cr
For individual asset plots:\cr
1 = Actual and fitted,\cr
2 = Actual vs. fitted,\cr
3 = Residuals vs. fitted, \cr
4 = Residuals with standard error bands, \cr
5 = Time series of squared residuals, \cr
6 = Time series of absolute residuals,\cr
7 = SACF and PACF of residuals,\cr
8 = SACF and PACF of squared residuals,\cr
9 = SACF and PACF of absolute residuals,\cr
10 = Non-parametric density of residuals with normal overlaid, \cr
11 = Non-parametric density of residuals with skew-t overlaid, \cr
12 = Histogram of residuals with non-parametric density and normal overlaid,\cr
13 = QQ-plot of residuals}

\item{f.sub}{numeric/character vector; subset of indexes/names of factors to
include for group plots. Default is 1:2.}

\item{a.sub}{numeric/character vector; subset of indexes/names of assets to
include for group plots. At least 2 assets must be selected. Default is 1:6.}

\item{plot.single}{logical; If \code{TRUE} plots the characteristics of an
individual asset's factor model. The type of plot is given by
\code{which}. Default is \code{FALSE}.}

\item{asset.name}{name of the individual asset to be plotted. Is necessary
if \code{x} contains multiple asset fits and \code{plot.single=TRUE}.}

\item{asset.variable}{the name of asset variable.}

\item{colorset}{color palette to use for all the plots. The 1st element will
be used for individual time series plots or the 1st object plotted, the 2nd
element for the 2nd object in the plot and so on.}

\item{legend.loc}{places a legend into one of nine locations on the chart:
"bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright",
"right", or "center". Default is "bottomright". Use \code{legend.loc=NULL}
to suppress the legend.}

\item{las}{one of {0, 1, 2, 3} to set the direction of axis labels, same as
in \code{plot}. Default is 1.}

\item{lwd}{set the line width, same as in \code{\link{plot}}. Default is 2.}

\item{maxlag}{optional number of lags to be calculated for ACF. Default is 15.}

\item{...}{further arguments to be passed to other plotting functions.}
}
\description{
Generic \code{plot} method for object of class \code{ffm}.
Plots chosen characteristic(s) for one or more assets.
}
\details{
The function can be used for group plots and individual plots. User can
select the type of plot either from the menu prompt (default) or directly
via argument \code{which}.

In case multiple plots are needed, the menu is repeated after each plot
(enter 0 to exit). User can also input a numeric vector of plot options via
\code{which}.

Group plots are the default. The selected assets in \code{a.sub} and
selected factors in \code{f.sub} are plotted depending on the
characteristic chosen. The default is to show the first 2 factors and first
6 assets.

Setting \code{plot.single=TRUE} enables individual plots. If there is more
than one asset fit by \code{x}, \code{asset.name} should be specified.
In case the \code{ffm} object \code{x} contains only a single asset fit,
plot.ffm can infer \code{asset.name} without user input.
}
\examples{

\dontrun{
# load data
data(stocksCRSP)
data(factorsSPGMI)

stocks_factors <- selectCRSPandSPGMI(stocks = stocksCRSP, factors = factorsSPGMI,
                                    dateSet = c("2006-01-31", "2010-12-31"),
                                    stockItems = c("Date", "TickerLast",
                                                   "CapGroup", "Sector",
                                                   "Return", "Ret13WkBill",
                                                   "mktIndexCRSP"),
                                    factorItems = c("BP", "LogMktCap", "SEV"),
                                    capChoice = "SmallCap",
                                    Nstocks = 20)

# fit a fundamental factor model with style variables BP and LogMktCap

fit.style <- fitFfm(data = stocks_factors,
                           asset.var = "TickerLast",
                           ret.var = "Return",
                           date.var = "Date",
                           exposure.vars = c("BP", "LogMktCap")
                           )

# For group plots (default), one can select plot option from prompt menu.
# The menu is repeated to produce multiple plots based on the same fit

# plot(fit.style) # Not run, but user should try it out


# plot all factor exposures from the last time period for 1st 10 assets

plot(fit.style, which = 2, f.sub = 1:2, a.sub = 1:10)

# plot factor model residuals scatterplot matrix, with histograms, density
# overlays, correlations and significance stars
plot(fit.style, which = 6)

# For individual plots: define `plot.single=TRUE` and specify `asset.name`.
# This will display a histogram of residuals from the asset's factor model fit

plot(fit.style, plot.single = TRUE, asset.name = "ALCO", which = 12)
}

}
\seealso{
\code{\link{fitFfm}}, \code{\link{residuals.ffm}},
\code{\link{fitted.ffm}}, \code{\link{fmCov.ffm}} and
\code{\link{summary.ffm}} for time series factor model fitting and related
S3 methods. Refer to \code{\link{fmSdDecomp}}, \code{\link{fmEsDecomp}},
\code{\link{fmVaRDecomp}} for factor model risk measures.

Here is a list of plotting functions used. (I=individual, G=Group)
I(1,5,6,7), G(3,4,12) - \code{\link[PerformanceAnalytics]{chart.TimeSeries}},
I(2,3,4,19), G(12) - \code{\link[graphics]{plot.default}},
I(3,4) - \code{\link[graphics]{panel.smooth}},
I(8,9,10) - \code{\link[PerformanceAnalytics]{chart.ACFplus}},
I(11,12) - \code{\link[stats]{plot.density}},
I(13) - \code{\link[PerformanceAnalytics]{chart.Histogram}},
I(14) - \code{\link[PerformanceAnalytics]{chart.QQPlot}},
I(15,16,17) - \code{\link[strucchange]{plot.efp}} (requires strucchange package),
I(18) - \code{\link[zoo]{plot.zoo}},
G(1) - \code{\link[PerformanceAnalytics]{chart.Boxplot}},
G(2,5,9,10,11) - \code{\link[lattice]{barchart}},
G(6) - \code{\link[PerformanceAnalytics]{chart.Correlation}} and
G(7,8) - \code{\link[corrplot]{corrplot.mixed}} (requires corrplot package).
}
\author{
Eric Zivot, Sangeetha Srinivasan and Yi-An Chen
}
