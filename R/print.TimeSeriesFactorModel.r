#' print TimeSeriesfactorModel object
#' 
#' Generic function of print method for fitTimeSeriesFactorModel.
#' 
#' 
#' @param fit.macro fit object created by fitTimeSeriesFactorModel.
#' @param digits. integer indicating the number of decimal places.  
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' fit.macro <- fitTimeseriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                       factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                       data=managers.df,fit.method="OLS")
#' print(fit.macro)
#' 
#' @export
print.TimeSeriesFactorModel <- function(fit.macro,digits=3){
n <- length(fit.macro$beta)
table.macro <-  as.matrix(fit.macro$alpha,nrow=n[1])
table.macro <- cbind(table.macro,fit.macro$beta,fit.macro$r2,fit.macro$resid.variance)
beta.names <- colnames(fit.macro$beta)
for (i in 1:length(beta.names)) {
beta.names[i] <- paste("beta.",beta.names[i],sep="")
}
colnames(table.macro) <- c("alpha",beta.names,"r2","resid.var")
print(round(table.macro,digits=digits))
}
