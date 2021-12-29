#' @title Select CRSP Stocks
#'
#' @description Select a set of CRSP stocks over a range of dates between
#' January 1993 and December 2015
#'
#' @details
#' The default values for dateSet, stockItems, capChoice and Nstocks can be
#' changed by the user. A user choice dateSet = NULL results selecting stocks
#' for the entire stocksCRSP time range from 1993-01-31 to 2015-12-32. A user
#' choice Nstocks = NULL, results in the selecting the maximum number of stocks
#' availabe subject to the value of capChoice. NOTE: In next revision of this
#' data set, the possible values of capChoice will be extended.
#' 
#' @importFrom utils globalVariables
#' 
#' @param stocks A data.table of stock returns and related data 
#' @param dateSet A character vector providing a start data and an end 
#' date, having the same form as c("2006-01-31", "2010-12-31")
#' @param stockItems A character vector that is a subset of the names
#' of stocks data.table
#' @param capChoice One of the market capitalization group names
#' "Smallcap", "Smallcap", "MidCap", "LargeCap".  
#' @param Nstocks Number of stocks in one of the marketcap groups, not
#' to exceed the number of stocks in the chosen group.
#'
#' @return A data.table object consisting of selected stocks
#' 
#'@examples
#'data(stocksCRSP)
#'
#'stocksDat <- selectCRSP(stocks = stocksCRSP, 
#'                        dateSet = c("2006-01-31", "2010-12-31"), 
#'                        stockItems = c("Date", "TickerLast", 
#'                                        "CapGroup", "Sector", 
#'                                        "Return", "Ret13WkBill",
#'                                        "mktIndexCRSP"),
#'                        capChoice = "SmallCap",
#'                        Nstocks = 20)
#'
#'str(stocksDat)
#'@export
selectCRSP <- function(stocks = stocksCRSP,
                       dateSet = c("2006-01-31","2010-12-31"), 
                       stockItems = c("Date", "TickerLast", "CapGroup",
                                      "Sector", "Return", "Ret13WkBill",
                                      "mktIndexCRSP"),
                       capChoice = "SmallCap",
                       Nstocks = 20)
{ 
  # as recommended to clear "no visible binding for global variable" build NOTE 
  CapGroup <- Date <- Sector <- TickerLast <- stocksCRSP <- NULL
  
  stockDat <- stocks
  colNames <- stockItems
  stockDat <- stockDat[, .SD , .SDcols = colNames]
  stockDat <- stockDat[!is.na(Sector)] # 250 stocks for now
  stockDat <- stockDat[CapGroup == capChoice]
  if(!is.null(dateSet)){
    stockDat <- stockDat[Date >= dateSet[1] & Date <= dateSet[2]]
  }
  if(!is.null(Nstocks)){
    tickerSet <- unique(stockDat[ ,TickerLast])[1:Nstocks]
    stockDat <- stockDat[TickerLast %in% tickerSet, ]
  }
  stockDat
}
