#' @title Select CRSP Stocks and SPGMI Factors
#'
#' @description Select and merge a set of CRSP stocks and SPGMI factors and
#' SPGMI factors over a range of dates between January 1993 and December 2015
#'
#' @details
#' The date range is specified by a beginning Date object and an ending Date 
#' object.  Default values for the stocksCRSP and scoresSPGMI component names,
#' marketcap group, and number of stocks in the marketcap group can be
#' changed by the user  
#' @param stocksCRSP xxx
#' @param scoresSPGMI xxx
#' @param dateSet xxx
#' @param stockItems xxx
#' @param factorItems xxx
#' @param capChoice xxx 
#' @param Nstocks xxx
#'
#' @return
#' @export
#'
#'
selectCRSPandSPGMI <- function(stocksCRSP,scoresSPGMI,
                               dateSet = c("2006-01-31","2010-12-31"), 
                               stockItems = c("Date","TickerLast","CapGroup","Sector","Return",
                                              "Ret13WkBill","mktIndexCRSP"),
                               factorItems = c("BP","LogMktCap","SEV"), 
                               capChoice = "SmallCap",
                               Nstocks = 20)
{ 
  commonNames <- intersect(names(stocksCRSP),names(scoresSPGMI))
  facModDat <- merge(stocksCRSP, scoresSPGMI, by = commonNames)
  itemsStocks <- stockItems
  itemsFactors <- factorItems
  colNames <- c(itemsStocks,itemsFactors)
  facModDat <- facModDat[, .SD , .SDcols = colNames]
  facModDat <- facModDat[!is.na(Sector)] # 250 stocks for now
  facModDat <- facModDat[CapGroup == capChoice]
  facModDat <-facModDat[Date >= dateSet[1] & Date <= dateSet[2]]
  tickerSet <- unique(facModDat[,TickerLast])[1:Nstocks]
  facModDat <- facModDat[TickerLast %in% tickerSet,]
  facModDat
}
