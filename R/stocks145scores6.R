#' stocks145scores6
#'
#' CRSP stocks Capital IQ scores
#' 
#' @description Contains returns for 145 stocks starting from Jan 1990 to Dec 2014 spanning
#'              ENERGY, COSTAP, INDUS,T MATRLS, FINS, INFOTK, HEALTH, CODISC, UTILS and TELCOM
#'              along with factors ROE, BP, MOM121, SIZE, VOL121, and EP.
#'
#'              The 10 Sectors correspond to Energy, ConsumerStaples, Industrials, Materials, Financials,
#'              InformationTechnology, HealthCare, ConsumerDiscretionary,
#'              Utilities and Telecommunications respectively.
#' @docType data
#'
#' @usage data('stocks145scores6')
#'
#' @format A data.frame with 43000 observations on 15 variables:
#' \itemize{
#'  \item \strong{DATE:} type `Date`
#'  \item \strong{compustat_id:} type `int`
#'  \item \strong{TICKER:} type `chr`
#'  \item \strong{NAME:} type `chr`
#'  \item \strong{GSECTOR:} type `num`
#'  \item \strong{SECTOR:} type `chr`
#'  \item \strong{ROE:} type `num`
#'  \item \strong{BP:} type `num`
#'  \item \strong{PM12M1M:} type `num`
#'  \item \strong{SIZE:} type `num`
#'  \item \strong{ANNVOL1M:} type `num`
#'  \item \strong{EP:} type `num`
#'  \item \strong{RETURNRAW:} type `num`
#'  \item \strong{RETURN:} type `num`
#'  \item \strong{rf:} type `num`
#' }
#' @source TBA
#' @examples  str(stocks145scores6)
"stocks145scores6"