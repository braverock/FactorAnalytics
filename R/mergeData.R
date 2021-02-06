#' Merge SPGMI scores with CRSP stocks.
#' 
#' @author Kirk Li.
#' 
#' @examples
#' data(stocksCRSP)
#' data(scoresSPGMI)
#' dataSPGMICRSP = mergeSPGMICRSP()
#' @export   
#' 

mergeSPGMICRSP = function(){

	merge(stocksCRSP,scoresSPGMI,by=c('Ticker','Date','Company.Name','CapGroup'))

}


