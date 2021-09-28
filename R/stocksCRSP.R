#' stocksCRSP
#'
#' CRSP monthly stocks data for 300 stocks 1993 to 2015
#' 
#' @docType data
#'
#' @usage data(stocksCRSP)
#'
#' @format A data.frame with 82000 observations on 15 variables:
#' \itemize{
#'  \item \strong{Date:} type `Date`.
#'  \item \strong{TickerLast:} type `chr`. This is the ticker as of the last date
#'  \item \strong{Ticker:} type `chr`. This is the actual ticker at each time
#'  period
#'  \item \strong{Company:} type `chr`. The name of company with TickerLast
#'  \item \strong{CapGroup:} type `chr`. The market capitalization group of the
#'  company, MicroCap, SmallCap, MidCap or LargeCap
#'  \item \strong{GICS:} type `chr`. 6 digit S&P GICS code
#'  \item \strong{Sector:} type `chr`. One of 10 sectors specified by the first
#'  two digits of the GICS code
#'  \item \strong{Return:} type `num`. Arithmetic stock return from one period
#'  to the next in decimal form
#'  \item \strong{RetExDiv:} type `num`.
#'  \item \strong{Price:} type `num`. Stock price at each time period in decimal
#'  form
#'  \item \strong{PrcSplitAdj:} type `num`.
#'  \item \strong{Ret4WkBill:} type `num`. Return of 4 week Treasury bill
#'  \item \strong{Ret13WkBill:} type `num`. Return of 13 week Treasury bill
#'  \item \strong{Ret1YrBill:} type `num`. Return of 1 year Treasury bill
#'  \item \strong{mktIndexCRSP:} type `num`. CRSP value weighted market return
#' }
#' 
#' @source Center for Research in Security Prices (CRSP) at the University of
#' Chicago's Booth School of Business (CRSP).
#' 
#' @section CRSP data
#' 
#' CRSP data is not covered by the GPL. Redistribution of the data is not
#' permitted, and use of the data in derivative works is not permitted without
#' the express written permission of CRSP.
#' 
#' @examples  
#' data(stocksCRSP)
#' names(stocksCRSP)
#' unique(stocksCRSP$Sector)
#' unique(stocksCRSP$CapGroup)
#' head(stocksCRSP,2)
"stocksCRSP"