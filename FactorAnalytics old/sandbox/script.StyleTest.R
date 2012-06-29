# Can style analysis work?
#   - Contrive some examples
#     - Sine wave portfolio of stocks and bonds
#     - Sine wave with short small-cap exposure
#     - Known portfolio with changing exposures
#   - Test some methods
#     - R^2 factor selection
#     - AIC factor selection
#     - quanreg
#   - Filters
#     - Notch filter
#     - Kalman filters
#   - Lo AP attribution

# Get packages and extra functions
source("../../../PerformanceAnalytics/R/chart.StackedBar.R")
# library(PerformanceAnalytics)
# library(qpcR) # for PRESS

# Get and arrange data
# ca.z = Return.read("ca-data.csv", freq="d")
R.assets = ca.z[1:120,c(25,30,45)]

# Create sine portfolio weights
w1=round((sin(seq(from=1, to=41,by=0.333333))+1)/2,4)
w.sine = as.xts(matrix(c(w1[1:120],1-w1[1:120], rep(-1, 120)), ncol=3), order.by = index(R.assets))
colnames(w.sine) = c("Lg Stock", "Bond", "Sm Stock")

# Calculate performance of sine portfolio
R.portf1 = w.sine[,1:2]*R.assets[,1:2]
# R.portf1 = w.sine*R.assets
R.portf = as.xts(matrix(rowSums(R.portf1)), order.by=index(R.portf1))
colnames(R.portf)= "Actual"
# Merge data for style analysis functions
merged.assets = na.omit(merge(R.portf, R.assets[,1:2]))
posn = barplot(w, plot=FALSE)


pdf(file="StyleTest.pdf", height=10, width=7.5, paper="letter")
# Page 1: Sine weights against 12-Month Constrained Sharpe Style analysis
    layout(matrix(c(1,2,3)),height=c(1,1,1),width=1)

    # Plot known weights of sine portfolio
    chart.StackedBar(w.sine[,1:2], border = "darkgray", main = "Known Portfolio Weights", legend.loc=NULL, las = 1, colorset = rainbow10equal)

    # Calculate 12-month rolling style
    result = rollapply(data = merged.assets, width = 12, FUN= function(x) {t(style.fit(R.fund = x[,1,drop=FALSE], R.style = x[,-1,drop=FALSE], method = "constrained", leverage = FALSE)$weights)}, by = 1, by.column = FALSE, na.pad = TRUE, align = "right")
    chart.StackedBar(result, main = "12-Month Rolling Constrained", legend.loc=NULL, border = "darkgray", las = 1, colorset = rainbow10equal, ylim=c(0,1))
    w.roll = rollapply(na.omit(w.sine[,1]), width = 12, FUN = "mean", na.pad = TRUE, align = "right")
    lines(posn,w.roll, col="blue")
    # chart.RollingStyle(R.fund = R.portf, R.style = R.assets[,1:2], leverage=FALSE, width=12, main = "12-Month Rolling Constrained", legend.loc=NULL, border = "darkgray", las = 1, colorset = rainbow10equal)

    R.fitted = xts(rowSums(as.xts(result)*R.assets[,1:2]), order.by=time(result))
    colnames(R.fitted) = "Fitted"
    chart.CumReturns(merge(R.portf,R.fitted), legend.loc = "topleft", colorset=c("black","darkblue"), main="Cumulative Performance")

# Page 2: Sine weights against 24-Month Constrained Sharpe Style analysis
    layout(matrix(c(1,2,3)),height=c(1,1,1),width=1)

    # Plot known weights of sine portfolio
    chart.StackedBar(w.sine[,1:2], border = "darkgray", main = "Known Portfolio Weights", legend.loc=NULL, las = 1, colorset = rainbow10equal)

    # Calculate 24-month rolling style
    result = rollapply(data = merged.assets, width = 24, FUN= function(x) {t(style.fit(R.fund = x[,1,drop=FALSE], R.style = x[,-1,drop=FALSE], method = "constrained", leverage = FALSE)$weights)}, by = 1, by.column = FALSE, na.pad = TRUE, align = "right")
    chart.StackedBar(result, main = "24-Month Rolling Constrained", legend.loc=NULL, border = "darkgray", las = 1, colorset = rainbow10equal, ylim=c(0,1))
    w.roll = rollapply(na.omit(w.sine[,1]), width = 24, FUN = "mean", na.pad = TRUE, align = "right")
    lines(posn,w.roll, col="blue")
    # chart.RollingStyle(R.fund = R.portf, R.style = R.assets[,1:2], leverage=FALSE, width=12, main = "12-Month Rolling Constrained", legend.loc=NULL, border = "darkgray", las = 1, colorset = rainbow10equal)

    R.fitted = xts(rowSums(as.xts(result)*R.assets[,1:2]), order.by=time(result))
    colnames(R.fitted) = "Fitted"
    chart.CumReturns(merge(R.portf,R.fitted), legend.loc = "topleft", colorset=c("black","darkblue"), main="Cumulative Performance")

# Page 3: Sine weights against 12-Month Constrained Sharpe Style analysis w Notch
    layout(matrix(c(1,2,3)),height=c(1,1,1),width=1)

    # Plot known weights of sine portfolio
    chart.StackedBar(w.sine[,1:2], border = "darkgray", main = "Known Portfolio Weights", legend.loc=NULL, las = 1, colorset = rainbow10equal)

    # Calculate 12-month rolling style
    result = rollapply(data = merged.assets, width = 12, FUN= function(x) {t(style.fit(R.fund = x[,1,drop=FALSE]*1*c(1:(12/2)/(12/4),1:(12/2):1/(12/4)), R.style = x[,-1,drop=FALSE], method = "constrained", leverage = FALSE)$weights)}, by = 1, by.column = FALSE, na.pad = TRUE, align = "right")
    chart.StackedBar(result, main = "12-Month Rolling Constrained w Notch", legend.loc=NULL, border = "darkgray", las = 1, colorset = rainbow10equal, ylim=c(0,1))
    w.roll = rollapply(na.omit(w.sine[,1]), width = 12, FUN = "mean", na.pad = TRUE, align = "right")
    lines(posn,w.roll, col="blue")
    # chart.RollingStyle(R.fund = R.portf, R.style = R.assets[,1:2], leverage=FALSE, width=12, main = "12-Month Rolling Constrained", legend.loc=NULL, border = "darkgray", las = 1, colorset = rainbow10equal)

    R.fitted = xts(rowSums(as.xts(result)*R.assets[,1:2]), order.by=time(result))
    colnames(R.fitted) = "Fitted"
    chart.CumReturns(merge(R.portf,R.fitted), legend.loc = "topleft", colorset=c("black","darkblue"), main="Cumulative Performance")
dev.off()


# par(mar=c(5,4,4,2))
# chart.StackedBar(w.sine[,1:2], border = "darkgray", main = "Known Portfolio Weights", legend.loc=NULL, las = 1, colorset = rainbow10equal)
# par(mar=c(5,4,4,2))
# chart.RollingStyle(R.fund = R.portf, R.style = R.assets[,1:2], leverage=FALSE, width=24, main = "24-Month Rolling Constrained", legend.loc=NULL, border = "darkgray", las = 1, colorset = rainbow10equal)
# dev.off()
# > c(1:(24/2)/(24/4),1:(24/2):1/(24/4))
#  [1] 0.1666667 0.3333333 0.5000000 0.6666667 0.8333333 1.0000000 1.1666667
#  [8] 1.3333333 1.5000000 1.6666667 1.8333333 2.0000000
# > 1:(12/2)/(12/4)
# [1] 0.3333333 0.6666667 1.0000000 1.3333333 1.6666667 2.0000000
# > (12/2)/(12/4):1
# [1] 2 3 6
# > (12/2):1/(12/4)
# merged.assets = na.omit(merge(R.portf, R.assets[,1:2]))
# result = rollapply(data = merged.assets, width = 24, FUN= function(x) {t(style.fit(R.fund = x[,1,drop=FALSE]*2*c(1:(24/2)/(24/4),1:(24/2):1/(24/4)), R.style = x[,-1,drop=FALSE], method = "constrained", leverage = FALSE)$weights)}, by = 1, by.column = FALSE, na.pad = FALSE, align = "right")
# 
#     colnames(result$weights) = columnnames.style
#     chart.StackedBar(result$weights)
