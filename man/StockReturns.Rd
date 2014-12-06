\name{StockReturns}
\alias{StockReturns}
\alias{r.M}
\alias{r.W}
\docType{data}
\title{Stock Return Data}
\description{
\code{r.M}: A "data.frame" object with monthly returns (ranging from January 1978 to December 1987) for 15 assets whose names are given in the 'Details'.
  
\code{r.W}: A "data.frame" object with weekly returns (ranging from January 8, 1997 to June 28, 2000) for 1618 U.S. stocks.
}
\details{
The 15 assets in \code{r.M} are as follows:
  CITCRP monthly returns of Citicorp.  
  CONED monthly returns of Consolidated Edison.  
  CONTIL monthly returns of Continental Illinois.  
  DATGEN monthly returns of Data General.  
  DEC monthly returns of Digital Equipment Company. 
  DELTA monthly returns of Delta Airlines.  
  GENMIL monthly returns of General Mills.  
  GERBER monthly returns of Gerber.  
  IBM monthly returns of International Business Machines.  
  MARKET a value-weighted composite monthly returns based on transactions from the New York Stock Exchange and the American Exchange.  
  MOBIL monthly returns of Mobile.  
  PANAM monthly returns of Pan American Airways.  
  PSNH monthly returns of Public Service of New Hampshire.  
  TANDY monthly returns of Tandy.  
  TEXACO monthly returns of Texaco.  
  WEYER monthly returns of Weyerhauser.  
  RKFREE monthly returns on 30-day U.S. Treasury bills.
}
\usage{data(StockReturns)}
\format{
data.frame object
\describe{
    \item{\code{r.M}}{monthly from Jan-1998 through Dec-1987}
    \item{\code{r.W}}{weekly from Jan-08-1997 through Jun-28-2000}
  }
}
\source{
  S+FinMetrics Berndt.dat & folio.dat
}
\references{
Berndt, E. R. (1991). The practice of econometrics: classic and contemporary. Reading, MA: Addison-Wesley.
}
\examples{
data(StockReturns)
dim(r.M)
range(rownames(r.M))
dim(r.W)
range(rownames(r.W))
}
\keyword{datasets}
