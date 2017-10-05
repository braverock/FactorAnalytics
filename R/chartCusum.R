#' @title cusumActMgr Plots
#'
#'
#' @description Plot the elements of an \code{cusumActMgr} object. The EWMA tracking errors are computed
#' month-to-month by the simple weighted average of the "log-excess returns" volatility and its former (adjacent) time period volatility. 
#' The excess volatility is the EWMA tracking error difference from the portfolio returns EWMA tracking error
#' and the benchmark returns EWMA tracking error. Annualized IR is computed by using the monthly log-excess returns
#' and the annualized EWMA tracking errors. 
#' 
#'
#' @importFrom zoo as.yearmon coredata index
#' @importFrom stats sd
#' @importFrom lattice xyplot
#' @importFrom MASS rlm
#'
#' @param object An object of class \code{cusumActMgr} returned by \code{cusumActMgr}.
#' @param digits The number of digits of numerical values in graphs
#' @param which A number or a vector of numbers to indicate the type of plots.
#' If a subset of the plots is required, specify a subset of the numbers 1 and 2
#' for plots. The numbers 1 and 2 represent: \cr \cr
#' 1 = Barplot of log-excess returns with annually moving average returns,
#' plot of tracking error, Plot of excess volatility, and Plot of cusum for returns
#' with protractors of slopes representing the annualized returns,\cr
#' 2 = Barplot of the information ratio, plot of cusumIR with protractors
#' of slopes with corresponding IR, and a plot of Lindley's Recursion with
#' the thresholds, and a scatter plot between the fund and benchmark returns
#' with robust regression\cr
#' @param ... other graphics parameters in plot
#'
#' @return
#' Graph(s) as specified by the user.
#'
#' @author Chindhanai Uthaisaad
#'
#' @examples
#' data("cusumData")
#' results = cusumActMgr(portfolioName = "Parvest", benchmarkName = "RUS2500", data = cusumData)
#' chartCusum(results, which = 1)
#' chartCusum(results, which = c(1,2))
#' @export


chartCusum <- function(object, digits = 3, which = NULL, ...) {

    options(digits = digits)

    which.vec <- which
    which <- which[1]

    repeat {
      switch(which,
             "1L" = {
               # 1: Plot of log-excess returns with annually moving average returns
               obj1 = 100 * object$Logarithmic_Excess_Returns
               obj2 = object$Information_Ratios
               obj3 = 100 * sqrt(12) * object$Tracking_Error
               obj4 = 100 * object$Excess_Volatility[,3]
               P1 = xyplot(obj1, ylab = "Excess Return (%)",
                           main = "Monthly Excess Return",
                           horizontal = FALSE, col = "blue", type = c("h", "g"), lwd = 3,
                           scales = list(y = list(rot = 0), axs='i',
                                         tick.number = 10),
                           xlab = "",
                           ylim = c(min(obj1)-1, max(obj1)+1),
                           panel = function(x, y,...) {
                            panel.xyplot(x, y,...)
                            panel.lines(100 * object$Annual_Moving_Average,
                                        col=2, lwd=2)
                           },
                           key = list(corner = c(1, 0.1),
                                     lines = list(col = 2, lty = 1, lwd = 2),
                                     text = list("12 Month Moving Average"),
                                     cex = 0.8))
               # 2: Plot of IR
               P2 = xyplot(obj2, main="Monthly Estimate: Annualized IR",
                           col=4, las=1, horizontal = FALSE, type = c('h', 'g'),
                           ylab = "IR", lwd = 1.75,
                           scales = list(y = list(rot = 0), axs='i', x = list(tick.number = 15)),
                           ylim = c(min(obj2)-1, max(obj2)+1),
                           panel=function(x,y,...){
                             panel.xyplot(x,y,...)
                             panel.abline(h = 0, col=1, lty = 1)})
               # 3: Plot of tracking error
               P3 = xyplot(obj3, main="EWMA Tracking Error (annualized)",
                           type = c('l', 'g'), las=0,
                           xlab = "", ylab = "Tracking Error (%)", col = 4, lwd = 1.5,
                           ylim = c(min(obj3)-1, max(obj3)+1),
                           scales = list(y = list(rot = 0), axs='i'), tick.number = 10)
               # 4: Excess volatility plot
               P4 = xyplot(obj4, main = "Excess Volatility", type = c('l', 'g'), col=4,
                           xlab = "", ylab = "Excess Volatility (%)", lwd = 1.5,
                           scales = list(y = list(rot = 0), axs='i', tick.number = 10),
                           ylim = c(min(obj4)-1, max(obj4)+1),
                           panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            panel.abline(h = 0, col = 4, lty = 3)})

               print(P1, split = c(1, 1, 2, 2), more = TRUE)
               print(P2, split = c(1, 2, 2, 2), more = TRUE)
               print(P3, split = c(2, 1, 2, 2), more = TRUE)
               print(P4, split = c(2, 2, 2, 2))

             },
             "2L" = {
               obj1 = object$Annualized_Cusum_ER
               obj2 = object$Annualized_Cusum_IR
               obj3 = -object$`Lindley's_Recursion`
               colors = c("firebrick4", "firebrick3", "firebrick2", 1,
                          "green4", "green3", "green2")
               # 1: CUSUM for returns
               P1 = xyplot(obj1, main="CUSUM Plot: Excess Returns",
                           las=2, col=4, ylab = "", lwd = 1.5, xlab = "",
                           scales = list(y = list(draw = FALSE), axs = 'i'),
                           ylim = c(min(obj1)-10, max(obj1)+10),
                           panel = function(x,y,...){
                             panel.xyplot(x,y,...)
                             for (i in 1:length(colors)){
                               panel.lines(object$Protractor_ER[,i], col=colors[i], lwd=1.5)
                             }
                             panel.lines(object$Protractor_ER[,4], col = 1, lwd = 1.5)
                           },
                           key = list(corner = c(0.97, 0),
                                      lines = list(col = colors, lty = 1, lwd = 1.5),
                                      text = list(c("ER = -3%","ER = -2%","ER = -1%", "ER = 0%",
                                                    "ER = 1%","ER = 2%","ER = 3%")),
                                      title = "Slopes on Protractor",
                                      cex = 0.6))

               colors1 = c("firebrick4", "firebrick3", "firebrick2", 1,
                          "green4", "green3", "green2")
               # 2: cusumIR
               P2 = xyplot(obj2, main="CUSUM Plot: Information Ratio",
                           xlab = "", col=4, lwd=1.5,
                           scales = list(y = list(draw = FALSE), axs = 'i'),
                           ylim = c(min(obj2)-5, max(obj2)+5),
                           panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            for (i in 1:length(colors1)){
                              panel.lines(object$Protractor_IR[,i], col=colors1[i], lwd=1.5)
                            }
                            panel.lines(object$Protractor_IR[,4], col=1, lwd=1.5)
                           },
                           key=list(corner = c(0.957,0),
                                   lines = list(col = colors1, lty=1, lwd=2),
                                   text = list(c("IR = -3","IR = -2","IR = -1", "IR = 0",
                                                 "IR = 1","IR = 2","IR = 3")),
                                   title = "Slopes on Protractor",
                                   cex = 0.6))

               horiz = c(-3.41, -4.33, -5.08, -5.72, -6.29, -6.81)
               colors2 = c(3, "green3", 7, "goldenrod1", "orangered", 2)
               ycoor = horiz + 0.2
               thresholds = c("24 | 16", "36 | 22", "48 | 27",
                              "60 | 32", "72 | 37","84 | 41")
               # 3: Lindley's Recursion
               P3 = xyplot(obj3, main="Underperformance LR Thresholds",
                           col=4, scales = list(y = list(rot = 0), axs = 'i'),
                           panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            for (i in 1:length(horiz)) {
                              panel.abline(h = horiz[i], col=colors2[i], lwd=2)
                            }
                            panel.abline(h = 0, col = 4, lty = 3)
                            panel.text(as.yearmon('2006-11', "%Y-%m"), y = -1.7, "Avg. Crossing Time", cex = 0.65)
                            panel.text(as.yearmon('2006-03', "%Y-%m"), y = -2.2, "IR = 0.5 | IR = 0", cex = 0.65)
                            for (i in 1:length(ycoor)) {
                              panel.text(as.yearmon('2006-03', "%Y-%m"), y = ycoor[i], thresholds[i], cex = 0.65)
                            }
                           })

               #Scatter plot with robust regression
               portRet = 100 * coredata(object$Means[,1])
               benchRet = 100 * coredata(object$Means[,2])
               Rob_lm = rlm(portRet ~ benchRet)
               Alpha = round(Rob_lm$coefficients[1], 3)
               Beta = round(Rob_lm$coefficients[2], 3)
               P4 = xyplot(portRet ~ benchRet, pch = 16, col = 4,
                          main = "Portfolio vs. Benchmark",
                          xlab = "Benchmark Returns (%)",
                          ylab = "Portfolio Returns (%)",
                          scales = list(y = list(rot = 0)),
                          panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            panel.abline(Rob_lm, col=2)
                            panel.abline(v=0, h=0, lty = 3)},
                          key=list(corner = c(0.92, 0.08),
                                   text = list(c(paste("Intercept =", 12 * Alpha, "% / annum"),
                                                 paste("Slope = ", Beta, ""))),
                                   title = "Linear Fit",
                                   cex = 0.7))

               print(P1, split=c(1,1,2,2), more=TRUE)
               print(P2, split=c(2,1,2,2), more=TRUE)
               print(P3, split=c(1,2,2,2), more=TRUE)
               print(P4, split=c(2,2,2,2))
             },
             invisible()
      )
      # repeat menu if user didn't choose to exit from the plot options
      if (which == 0 || length(which.vec) == 1) {
        break
      }
      if (length(which.vec) > 1) {
        which.vec <- which.vec[-1]
        which <- which.vec[1]
        par(ask = TRUE)
      } else {
        which = NULL
      }
    }
    par(ask = FALSE)
}
