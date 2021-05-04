#' download FA dataset from github
#'
#' @author Kirk Li.
#' @param filename stockCRSPdaily, stockCRSPweekly, stockCRSPmonthly
#'
#' @return
#' @export
#'
#' @examples
#' readGithubFAdata(filename  = "stockCRSPweekly")
#' 
readGithubFAdata <- function(filename = ''){

	file_url <- paste0("https://github.com/kecoli/FactorAnalyticsData/blob/master/data/",filename,".rda?raw=True")
	
	load(url(file_url),envir = .GlobalEnv)
	
	print(paste0('file ',filename,' is downloaded and loaded from github.'))
	
}




