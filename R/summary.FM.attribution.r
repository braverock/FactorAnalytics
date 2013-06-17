# summary.FM.attribution.r 
# Yi-An Chen
# 8/1/2012 

summary.FM.attribution <- function(fm.attr) {
   lapply(fm.attr[[3]],summary) 
}