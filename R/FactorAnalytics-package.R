

#' Functions for Cornish-Fisher density, CDF, random number simulation and
#' quantile.
#' 
#' \code{dCornishFisher} Computes Cornish-Fisher density from two term
#' Edgeworth expansion given mean, standard deviation, skewness and excess
#' kurtosis. \code{pCornishFisher} Computes Cornish-Fisher CDF from two term
#' Edgeworth expansion given mean, standard deviation, skewness and excess
#' kurtosis. \code{qCornishFisher} Computes Cornish-Fisher quantiles from two
#' term Edgeworth expansion given mean, standard deviation, skewness and excess
#' kurtosis. \code{rCornishFisher} simulate observations based on
#' Cornish-Fisher quantile expansion given mean, standard deviation, skewness
#' and excess kurtosis.
#' 
#' CDF(q) = Pr(sqrt(n)*(x_bar-mu)/sigma < q)
#' 
#' @aliases rCornishFisher dCornishFisher pCornishFisher qCornishFisher
#' @param n scalar, number of simulated values in rCornishFisher. Sample length
#' in density,distribution,quantile function.
#' @param sigma scalar, standard deviation.
#' @param skew scalar, skewness.
#' @param ekurt scalar, excess kurtosis.
#' @param seed set seed here. Default is \code{NULL}.
#' @param x,q vector of standardized quantiles. See detail.
#' @param p vector of probabilities.
#' @return n simulated values from Cornish-Fisher distribution.
#' @author Eric Zivot and Yi-An Chen.
#' @references A.DasGupta, "Asymptotic Theory of Statistics and Probability",
#' Springer Science+Business Media,LLC 2008 Thomas A.Severini, "Likelihood
#' Methods in Statistics", Oxford University Press, 2000
#' @examples
#' 
#' # generate 1000 observation from Cornish-Fisher distribution
#' rc <- rCornishFisher(1000,1,0,5)
#' hist(rc,breaks=100,freq=FALSE,main="simulation of Cornish Fisher Distribution",
#'       xlim=c(-10,10))
#' lines(seq(-10,10,0.1),dnorm(seq(-10,10,0.1),mean=0,sd=1),col=2) 
#' # compare with standard normal curve
#' 
#' # example from A.dasGupta p.188 exponential example
#' # x is iid exp(1) distribution, sample size = 5
#' # then x_bar is Gamma(shape=5,scale=1/5) distribution
#' q <- c(0,0.4,1,2)
#' # exact cdf 
#' pgamma(q/sqrt(5)+1,shape=5,scale=1/5)
#' # use CLT
#' pnorm(q)
#' # use edgeworth expansion
#' pCornishFisher(q,n=5,skew=2,ekurt=6)
#' 
#' @name CornishFisher
NULL





#' Hypothetical Alternative Asset Manager and Benchmark Data
#' 
#' a data.frame format from managers dataset from package PerformanceAnalytics,
#' containing columns of monthly returns for six hypothetical asset managers
#' (HAM1 through HAM6), the EDHEC Long-Short Equity hedge fund index, the S\&P
#' 500 total returns. Monthly returns for all series end in December 2006 and
#' begin at different periods starting from January 1997.
#' 
#' 
#' @name managers.df
#' @docType data
#' @keywords datasets
#' @examples
#' 
#' data(managers.df)
#' ## maybe str(managers.df) ; plot(managers.df) ...
#' 
NULL





#' Monthly Stock Return Data || Portfolio of Weekly Stock Returns
#' 
#' sfm.dat: This is a monthly "data.frame" object from January 1978 to December
#' 1987, with seventeen columns representing monthly returns of certain assets,
#' as in Chapter 2 of Berndt (1991).  sfm.apca.dat: This is a weekly
#' "data.frame" object with dimension 182 x 1618, which runs from January 8,
#' 1997 to June 28, 2000 and represents the stock returns on 1618 U.S. stocks.
#' 
#' CITCRP monthly returns of Citicorp.  CONED monthly returns of Consolidated
#' Edison.  CONTIL monthly returns of Continental Illinois.  DATGEN monthly
#' returns of Data General.  DEC monthly returns of Digital Equipment Company.
#' DELTA monthly returns of Delta Airlines.  GENMIL monthly returns of General
#' Mills.  GERBER monthly returns of Gerber.  IBM monthly returns of
#' International Business Machines.  MARKET a value-weighted composite monthly
#' returns based on transactions from the New York Stock Exchange and the
#' American Exchange.  MOBIL monthly returns of Mobile.  PANAM monthly returns
#' of Pan American Airways.  PSNH monthly returns of Public Service of New
#' Hampshire.  TANDY monthly returns of Tandy.  TEXACO monthly returns of
#' Texaco.  WEYER monthly returns of Weyerhauser.  RKFREE monthly returns on
#' 30-day U.S. Treasury bills.
#' 
#' @name stat.fm.data
#' @aliases sfm.dat sfm.apca.dat
#' @docType data
#' @references Berndt, E. R. (1991). The Practice of Econometrics: Classic and
#' Contemporary. Addison-Wesley Publishing Co.
#' @source S+FinMetrics Berndt.dat & folio.dat
#' @keywords datasets
NULL





#' constructed NYSE 447 assets from 1996-01-01 through 2003-12-31.
#' 
#' constructed NYSE 447 assets from 1996-01-01 through 2003-12-31.
#' 
#' Continuous data: PRICE, RETURN, VOLUME, SHARES.OUT, MARKET.EQUITY,LTDEBT,
#' NET.SALES, COMMON.EQUITY, NET.INCOME, STOCKHOLDERS.EQUITY, LOG.MARKETCAP,
#' LOG.PRICE, BOOK2MARKET Categorical data: GICS, GICS.INDUSTRY, GICS.SECTOR
#' Identication data: DATE, PERMNO, TICKER.x
#' 
#' @name stock
#' @docType data
#' @references Guy Yullen and Yi-An Chen
#' @keywords datasets
NULL



