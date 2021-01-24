#' download CRSP dataset from github
#'
#' @author Kirk Li.
#' @param filename stockCRSPdaily, stockCRSPweekly, stockCRSPmonthly
#'
#' @return
#' @export
#'
#' @examples
#' readCRSPdataGithub("stockCRSPweekly")
#' 
readCRSPdataGithub <- function(filename = ''){

	file_url <- paste0("https://github.com/braverock/FactorAnalytics/blob/master/data_nonpackage/",filename,".rda?raw=True")
	load(url(file_url))
	
	print(paste0('file',filename,' downloaded and loaded from github.'))
}






