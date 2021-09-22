#' @import xts
#' @import zoo
#' @import foreach
#' @import rrcov

#' @importFrom PerformanceAnalytics checkData Return.cumulative 
#' @importFrom RobStatTM covRob lmrobdetMM step.lmrobdetMM covClassic
#' @importFrom leaps regsubsets
#' @importFrom lars lars cv.lars
#' @importFrom sandwich vcovHC.default vcovHAC.default
#' @importFrom tseries tsbootstrap
#' @importFrom robustbase scaleTau2 covOGK

#' @importFrom PerformanceAnalytics chart.TimeSeries chart.ACFplus 
#' chart.Histogram chart.QQPlot chart.Correlation
#' @importFrom lattice barchart xyplot panel.barchart panel.grid
#' @importFrom corrplot corrplot.mixed

#' @importFrom parallel makeCluster detectCores clusterEvalQ clusterExport 
#' stopCluster
#' @importFrom boot boot
#' @importFrom doSNOW registerDoSNOW 
#' @importFrom RCurl merge.list
#' @importFrom bestglm bestglm

NULL