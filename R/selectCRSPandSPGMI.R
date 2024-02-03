#' @title Select CRSP Stocks and SPGMI Factors
#'
#' @description Select and merge a set of CRSP stocks and SPGMI factors and
#' SPGMI factors over a range of dates between January 1993 and December 2015
#'
#' @details
#' The date range is specified by a beginning Date object and an ending Date
#' object.  Default values for the stocks and factors component names,
#' marketcap group, and number of stocks in the marketcap group can be
#' changed by the user
#'
#' @importFrom utils globalVariables
#'
#' @param stocks A data.table of stock returns and related data
#' @param factors A data.table of factors and related data
#' @param dateSet A character vector providing a start data and an end
#' date, having the same form as c("2006-01-31", "2010-12-31")
#' @param stockItems A character vector that is a subset of the names
#' of stocks data.table
#' @param factorItems A character vector that is a subset of the names
#' of factors data.table
#' @param capChoice One of the market capitalization group names
#' "Smallcap", "Smallcap", "MidCap", "LargeCap".
#' @param Nstocks Number of stocks in one of the marketcap groups, not
#' to exceed the number of stocks in the chosen group.
#'
#' @return A merged data.table consisting of selected stocks and factors
#'
#'@examples
#'
#'\dontrun{
#'data(stocksCRSP)
#'data(factorsSPGMI)
#'
#'stocks_factors <- selectCRSPandSPGMI(stocks = stocksCRSP, factors = factorsSPGMI,
#'                                     dateSet = c("2006-01-31", "2010-12-31"),
#'                                     stockItems = c("Date", "TickerLast",
#'                                                    "CapGroup", "Sector",
#'                                                    "Return", "Ret13WkBill",
#'                                                    "mktIndexCRSP"),
#'                                     factorItems = c("BP", "LogMktCap", "SEV"),
#'                                     capChoice = "SmallCap",
#'                                     Nstocks = 20)
#'
#'str(stocks_factors)
#'}
#'@export
selectCRSPandSPGMI <- function(stocks = stocksCRSP, factors = factorsSPGMI,
                               dateSet = c("2006-01-31","2010-12-31"),
                               stockItems = c("Date", "TickerLast", "CapGroup",
                                              "Sector", "Return", "Ret13WkBill",
                                              "mktIndexCRSP"),
                               factorItems = c("BP", "LogMktCap", "SEV"),
                               capChoice = "SmallCap",
                               Nstocks = 20)
{

  # as recommended to clear "no visible binding for global variable" build NOTE
  CapGroup <- Date <- Sector <- TickerLast <- factorsSPGMI <- stocksCRSP <- NULL

  commonNames <- intersect(names(stocks), names(factors))
  facModDat <- merge(stocks, factors, by = commonNames)
  itemsStocks <- stockItems
  itemsFactors <- factorItems
   colNames <- c(itemsStocks, itemsFactors)
  facModDat <- facModDat[, .SD , .SDcols = colNames]
  facModDat <- facModDat[!is.na(Sector)] # 250 stocks for now
  facModDat <- facModDat[CapGroup == capChoice]
  facModDat <- facModDat[Date >= dateSet[1] & Date <= dateSet[2]]
  tickerSet <- unique(facModDat[ ,TickerLast])[1:Nstocks]
  facModDat <- facModDat[TickerLast %in% tickerSet, ]
  facModDat
}
