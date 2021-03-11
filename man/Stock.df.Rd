\docType{data}
\name{Stock.df}
\alias{Stock.df}
\alias{stock}
\title{Fundamental and return data for 447 NYSE stocks}
\description{
  Fundamental and return data:
  Assets: 447 stocks listed on the NYSE 
  Frequency: Monthly 
  Date range: 1996-02-29 through 2003-12-31
}
\details{
  Date variable: DATE
  
  Stock ID: TICKER
  
  Stock return and price variables: RETURN, PRICE
  
  Numeric exposures: VOLUME, SHARES.OUT, MARKET.EQUITY, 
  LTDEBT, NET.SALES, COMMON.EQUITY, NET.INCOME, STOCKHOLDERS.EQUITY, 
  LOG.MARKETCAP, LOG.PRICE, BOOK2MARKET
  
  Note: Numeric exposures are standardized as z-scores.
  
  Categorical variables: GICS, GICS.INDUSTRY, GICS.SECTOR
}
\usage{
data(Stocks.df)
}
\format{
data.frame
}
\examples{
data(Stocks.df)
str(stock)
}
\keyword{datasets}
\keyword{data.frame}
