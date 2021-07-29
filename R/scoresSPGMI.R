#' scoresSPGMI
#'
#' Cleaned SPGMI data containing 300 stocks' 12 factor scores data
#' 
#' @docType data
#'
#' @usage data('factorDataSetDjia5Yrs')
#'
#' @format A data.frame with 82800 observations on 21 variables:
#' \itemize{
#'  \item \strong{Date:} type `Date`
#'  \item \strong{TickerLast:} type `int`
#'  \item \strong{Ticker:} type `chr`
#'  \item \strong{Company:} type `chr`
#'  \item \strong{CapGroup:} type `chr`
#'  \item \strong{GICS:} type `chr`
#'  \item \strong{Sector:} type `chr`
#'  \item \strong{AnnVol12M:} type `num`
#'  \item \strong{Beta60M:} type `num`
#'  \item \strong{BP:} type `num`
#'  \item \strong{EP:} type `num`
#'  \item \strong{LogMktCap:} type `num`
#'  \item \strong{PM12M1M:} type `num`
#'  \item \strong{CFROIC:} type `num`
#'  \item \strong{Chg1YAstTo:} type `num`
#'  \item \strong{EBITDAEV:} type `num`
#'  \item \strong{FCFP:} type `num`
#'  \item \strong{PM1M:} type `num`
#'  \item \strong{SEV:} type `num`
#' }
#' #' @source S&P Global Market Intelligence
#' @examples  str(scoresSPGMI)
"scoresSPGMI"