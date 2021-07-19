
#Load the data
data("stocks145scores6")
dat = stocks145scores6
returns = tapply(dat$RETURN,list(dat$DATE,dat$TICKER),I)
ret = xts(returns[,1:5],as.yearmon(rownames(returns)))

#generate return time series plot               
tsPlotMP(ret, color = 'Blue')
tsPlotMP(ret, scaleType = "same", zeroLine = FALSE)
tsPlotMP(ret, stripLeft = FALSE, main = 'Time Series Plot')
tsPlotMP(ret, stripLeft = FALSE, main = 'Time Series Plot', layout = c(3,3))