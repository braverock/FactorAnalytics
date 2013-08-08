#'@name  CornishFisher
#'@aliases CornishFisher
#'@aliases rCornishFisher 
#'@aliases dCornishFisher
#'@aliases qCornishFisher
#'@aliases pCornishFisher
#' @export 

qCornishFisher <-
function(p,n,skew, ekurt) {
zq = qnorm(p)
q.cf = zq  + 1/sqrt(n)* (((zq^2 - 1) * skew)/6) + 1/n*((((zq^3 - 3 * zq) *
      ekurt)/24) - ((((2 * zq^3) - 5 * zq) * skew^2)/36) )
return(q.cf)
  
  
}

