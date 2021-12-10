#' factorsSPGMI
#'
#' 14 SPGMI monthly scores (alpha factors) for 300 stocks 1993 to 2015
#' 
#' @docType data
#'
#' @usage data('factorsSPGMI')
#'
#' @format A data.frame with 82800 observations on 21 variables:
#' \itemize{
#'  \item \strong{Date:} type `Date`.
#'  \item \strong{TickerLast:} type `chr`. This is the ticker at the last date
#'  \item \strong{Ticker:} type `chr`. This is the actual ticker at each month
#'  \item \strong{Company:} type `chr`. The name of company with TickerLast
#'  \item \strong{CapGroup:} type `chr`. The market capitalization group of the
#'  company, MicroCap, SmallCap, MidCap or LargeCap
#'  \item \strong{GICS:} type `chr`. 6 digit S&P GICS code
#'  \item \strong{Sector:} type `chr`. One of 10 sectors specified by the first
#'  two digits of the GICS code
#'  \item \strong{AnnVol12M:} type `num`. Stock nnualized 12 month volatility
#'  \item \strong{Beta60M:} type `num`. Stock beta computed using 60 months
#'  \item \strong{BP:} type `num`. Stock book to price ratio
#'  \item \strong{EP:} type `num`. Stock earnings to price ratio
#'  \item \strong{LogMktCap:} type `num`. Log of stock market cap in $M
#'  \item \strong{PM12M1M:} type `num`. Stock return from previous 12 months to
#'  previous 1 month
#'  \item \strong{CFROIC:} type `num`. Ratio of company's cash flow to return on
#'  invested capital
#'  \item \strong{Chg1YAstTo:} type `num`. One year change in sales turnover
#'  \item \strong{EBITDAEV:} type `num`. Ratio of EBITDA to enterprise value
#'  \item \strong{FCFP:} type `num`. Ratio of free cash flow to price
#'  \item \strong{PM1M:} type `num`. Stock return over previous one month
#'  \item \strong{SEV:} type `num`. Ratio of sales to enterprise value
#' }
#' 
#' @keywords datasets
#' 
#' @source Standard and Poors Global Market Intelligence (SPGMI). NOTE: SPGMI
#' data is not covered by the GPL. Redistribution of the SPGMI data is not
#' permitted, and use of the data in derivative works is not permitted without
#' the written permission of CRSP.
#' 
#' @examples  
#' data(factorsSPGMI)
#' str(factorsSPGMI)
#' head(factorsSPGMI, 5)
"factorsSPGMI"