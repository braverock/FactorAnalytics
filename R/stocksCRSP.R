#' stocksCRSP
#'
#' CRSP stocks data
#' 
#' @docType data
#'
#' @usage data('stocksCRSP')
#'
#' @format A data.frame with 82000 observations on 15 variables:
#' \itemize{
#'  \item \strong{Date:} type `Date`
#'  \item \strong{TICKERLast:} type `chr`. This is the ticker as of the last date
#'  \item \strong{Ticker:} type `chr`
#'  \item \strong{Company:} type `chr`
#'  \item \strong{CapGroup:} type `chr`
#'  \item \strong{GICS:} type `chr`
#'  \item \strong{SECTOR:} type `chr`
#'  \item \strong{Return:} type `num`
#'  \item \strong{BetExDiv:} type `num`
#'  \item \strong{Price:} type `num`
#'  \item \strong{PrcSplitAdj:} type `num`
#'  \item \strong{Ret4WkBill:} type `num`
#'  \item \strong{Ret13WkBill:} type `num`
#'  \item \strong{Ret1YrBill:} type `num`
#'  \item \strong{mktIndexCRSP:} type `num`
#' }
#' @source TBA
#' @examples  str(stocksCRSP)
"stocksCRSP"