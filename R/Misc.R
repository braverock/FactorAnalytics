#' @title Miscellaneous Imported functions
#' 
#' @details Only unique directives are saved to the ‘NAMESPACE’ file, so one 
#' can repeat them as needed to maintain a close link between the functions 
#' where they are needed and the namespace file. 
#' 
#' @importFrom PerformanceAnalytics checkData VaR chart.TimeSeries chart.ACFplus
#' chart.Histogram chart.QQPlot Return.cumulative
#' @importFrom robust lmRob step.lmRob
#' @importFrom leaps regsubsets
#' @importFrom lars lars cv.lars
#' @importFrom lmtest coeftest.default
#' @importFrom sandwich vcovHC.default vcovHAC.default
#' @importFrom lattice barchart panel.barchart panel.grid
#' @importFrom corrplot corrplot
#' @importFrom strucchange efp
#' @importFrom MASS ginv 