# summary.FM.attribution.r 
# Yi-An Chen
# 8/1/2012 



#' summary FM.attribution object.
#' 
#' Generic function of summary method for factorModelPerformanceAttribution.
#' 
#' 
#' @param fm.attr FM.attribution object created by
#' factorModelPerformanceAttribution.
#' @author Yi-An Chen.
#' @examples
#' 
#'   \dontrun{
#'   fm.attr <- factorModelPerformanceAttribution(fit.macro)
#'   summary(fm.attr)
#'   }
#'   
#' 
summary.FM.attribution <- function(fm.attr) {
   lapply(fm.attr[[3]],summary) 
}
