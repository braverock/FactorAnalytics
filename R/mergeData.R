#' Merge SPGMI scores with CRSP stocks
#' 
#' @param stocksCRSP a data.frame contains stocks in CRSP format. Default = 
#' stocksCRSP. See \link{stocksCRSP}
#' @param scoresSPGMI a data.frame contains factors in SPGMI format. Default =
#' scoresSPGMI. See \link{scoresSPGMI}
#' 
#' @author Kirk Li.
#' 
#' @examples
#' data(stocksCRSP, package = "FactorAnalytics")
#' data(scoresSPGMI, package = "FactorAnalytics")
#' dataSPGMICRSP = mergeSPGMICRSP()
#' @export   
#' 

mergeSPGMICRSP = function(){
  
	merge(stocksCRSP = stocksCRSP,
	      scoresSPGMI = scoresSPGMI,
	      by = intersect( names(stocksCRSP), names(scoresSPGMI) ),
	      sort = FALSE)

  }


