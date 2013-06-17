\name{plot.StatFactorModel}
\alias{plot.StatFactorModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot StatFactorModel object.
}
\description{
Generic function of plot method for fitStatisticFactorModel. Either plot all fit models or choose a single asset to plot. 
}
\usage{
\method{plot}{StatFactorModel}(fit.stat, variables, cumulative = TRUE, style = "bar",
         which.plot = c("none","1L","2L","3L","4L","5L","6L","7L","8L"),
         hgrid = FALSE, vgrid = FALSE, plot.single=FALSE, fundName,
         which.plot.single=c("none","1L","2L","3L","4L","5L","6L",
                             "7L","8L","9L","10L","11L","12L","13L"), ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit.stat}{
fit object created by fitStatisticalFactorModel.
}
  \item{variables}{
Optional. an integer vector telling which variables are to be plotted. The default is to plot all the variables, or the number of variables explaining 90 percent of the variance, whichever is bigger.
}
  \item{cumulative}{
a logical flag: if TRUE, the cumulative fraction of the variance is printed above each bar in the plot. 
}
\item{style}{
Charater. bar or lines can be chosen. 
}
\item{which.plot}{
integer indicating which plot to create: "none" will create a menu to choose. Defualt is none.
1 = "Screeplot of Eigenvalues",
2 = "Factor returns",
3 = "FM Correlation",
4 =   "R square",
5 = "Variance of Residuals",
6 = "Factor Contributions to SD",
7 = "Factor Contributions to ES",
8 = "Factor Contributions to VaR"
}
\item{hgrid}{
Logic. Whether to plot horizontal grid or not. Defualt is FALSE.  
}
\item{vgrid}{
Logic. Whether to plot vertical grid or not. Defualt is FALSE.
}
\item{plot.single}{
Plot a single asset of lm class. Defualt is FALSE.
}
\item{fundName}{
Name of the asset to be plotted. 
}             
\item{which.plot.single}{
integer indicating which plot to create:
"none" will create a menu to choose. Defualt is none.
1 =    time series plot of actual and fitted values
2 =    time series plot of residuals with standard error bands
3 =    time series plot of squared residuals
4 =    time series plot of absolute residuals
5 =    SACF and PACF of residuals
6 =    SACF and PACF of squared residuals
7 =    SACF and PACF of absolute residuals
8 =    histogram of residuals with normal curve overlayed
9 =    normal qq-plot of residuals
10=    CUSUM plot of recursive residuals
11=    CUSUM plot of OLS residuals
12=    CUSUM plot of recursive estimates relative to full sample estimates
13=    rolling estimates over 24 month window
}
\item{...}{
other variables for barplot method.
}
}
\details{
PCA works well. APCA is underconstruction.
}
\author{
Eric Zivot and Yi-An Chen.
}

\examples{
\dontrun{
# load data for fitStatisticalFactorModel.r
# data from finmetric berndt.dat and folio.dat

data(stat.fm.data)
# pca
sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat,k=10)
args(plot.StatFactorModel)
# plot all
plot(sfm.pca.fit)
# plot single asset
plot(sfm.pca.fit,plot.single=TRUE,fundName="CITCRP")
}
}