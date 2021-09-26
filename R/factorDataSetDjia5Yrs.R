#' factorDataSetDjia5Yrs
#'
#' DJIA stocks Compustat factors 5 yrs. Data loads lazily
#' 
#' @description Contains returns for 30 DJIA stocks spanned across 9 Sectors -ENERGY, COSTAP, INDUS,T MATRLS,
#'              FINS, INFOTK, HEALTH, CODISC, and TELCOM stocks along with 4 factor data (MKTCAP, ENTVAL, P2B, EV2S, SIZE)
#'              starting from from Jan 2008 to Dec 2012.
#'
#'              The 9 Sectors correspond to Energy,ConsumerStaples, Industrials, Materials, Financials,
#'              InformationTechnology, HealthCare, ConsumerDiscretionary and Telecommunications respectively.
#' 
#' @docType data
#'
#' @usage data('factorDataSetDjia5Yrs')
#'
#' @format A data.frame with 1320  observations on 17 variables:
#' \itemize{
#'  \item \strong{DATE:} type `yearmon`
#'  \item \strong{PERMNO:} type `int`
#'  \item \strong{GVKEY:} type `int`
#'  \item \strong{CUSIP:} type `chr`
#'  \item \strong{TICKER:} type `chr`
#'  \item \strong{RETURN:} type `num`
#'  \item \strong{RETURN.OLD:} type `num`
#'  \item \strong{RETURN:} type `num`
#'  \item \strong{RETURN.DIFF:} type `num`
#'  \item \strong{GSECTOR:} type `num`
#'  \item \strong{SECTORNAMES:} type `chr`
#'  \item \strong{SECTOR:} type `chr`
#'  \item \strong{GSECTOR:} type `num`
#'  \item \strong{ENTVAL:} type `num`
#'  \item \strong{P2B:} type `num`
#'  \item \strong{EV2S:} type `num`
#'  \item \strong{SIZE:} type `num`
#' }
#' @source TBA
#' @examples  
#' data(factorDataSetDjia5Yrs)
#'  str(factorDataSetDjia5Yrs)
"factorDataSetDjia5Yrs"