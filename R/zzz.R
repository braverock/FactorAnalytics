#' @import xts
#' @import zoo
#' @import foreach

#' @importFrom PerformanceAnalytics checkData VaR Return.cumulative 
#' @importFrom robust lmRob step.lmRob
#' @importFrom leaps regsubsets
#' @importFrom lars lars cv.lars
#' @importFrom lmtest coeftest.default
#' @importFrom sandwich vcovHC.default vcovHAC.default
#' @importFrom MASS ginv

#' @importFrom PerformanceAnalytics chart.TimeSeries chart.ACFplus 
#' chart.Histogram chart.QQPlot chart.Correlation
#' @importFrom lattice barchart xyplot panel.barchart panel.grid
#' @importFrom corrplot corrplot.mixed
#' @importFrom strucchange efp
#' @importFrom sn dst st.mple

#' @importFrom parallel makeCluster detectCores clusterEvalQ clusterExport 
#' stopCluster
#' @importFrom boot boot
#' @importFrom doSNOW registerDoSNOW 
#' @importFrom RCurl merge.list
#' @importFrom bestglm bestglm

NULL