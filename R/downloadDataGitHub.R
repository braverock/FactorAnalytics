#' download FA dataset from github
#'
#' @author Kirk Li.
#' @param filename stockCRSPdaily, stockCRSPweekly, stockCRSPmonthly
#'
#' @return
#' @export
#'
#' @examples
#' readGithubFAdata("stockCRSPweekly")
#' 
readGithubFAdata <- function(filename = ''){

	file_url <- paste0("https://github.com/braverock/FactorAnalyticsData/blob/master/data/",filename,".rda?raw=True")
	load(url(file_url))
	
	print(paste0('file',filename,' downloaded and loaded from github.'))
}






