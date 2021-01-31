#' @import xts
#' @import zoo
#' @import foreach
#' @import rrcov

#' @importFrom PerformanceAnalytics checkData Return.cumulative 
#' @importFrom robust covRob lmRob step.lmRob covClassic
#' @importFrom leaps regsubsets
#' @importFrom lars lars cv.lars
#' @importFrom lmtest coeftest.default
#' @importFrom sandwich vcovHC.default vcovHAC.default
#' @importFrom MASS ginv
#' @importFrom tseries tsbootstrap
#' @importFrom robustbase scaleTau2 covOGK

#' @importFrom PerformanceAnalytics chart.TimeSeries chart.ACFplus 
#' chart.Histogram chart.QQPlot chart.Correlation
#' @importFrom lattice barchart xyplot panel.barchart panel.grid
#' @importFrom corrplot corrplot.mixed
#' @importFrom strucchange efp
#' @importFrom sn dst rst st.mple

#' @importFrom parallel makeCluster detectCores clusterEvalQ clusterExport 
#' stopCluster
#' @importFrom boot boot
#' @importFrom doSNOW registerDoSNOW 
#' @importFrom RCurl merge.list
#' @importFrom bestglm bestglm

NULL