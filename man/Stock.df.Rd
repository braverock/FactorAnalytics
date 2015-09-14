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
  ID variables: DATE, TICKER
  
  Continuous variables: RETURN, PRICE, VOLUME, SHARES.OUT, MARKET.EQUITY, 
  LTDEBT, NET.SALES, COMMON.EQUITY, NET.INCOME, STOCKHOLDERS.EQUITY, 
  LOG.MARKETCAP, LOG.PRICE, BOOK2MARKET 
  
  Categorical variables: GICS, GICS.INDUSTRY, GICS.SECTOR
}
\usage{
data(Stock.df)
}
\format{
data.frame
}
\examples{
data(Stock.df)
str(stock)
}
\keyword{datasets}
\keyword{data.frame}
