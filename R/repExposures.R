#' @title Portfolio Exposures Report
#' 
#' @description Calculate k factor time series based on fundamental factor model. This method takes fundamental factor model fit, 'ffm' object, and portfolio weight as inputs and generates numeric summary and plot visualization. 
#' 
#' @importFrom zoo as.yearmon coredata index
#' @importFrom graphics boxplot 
#' @importFrom stats sd
#' @importFrom utils menu
#' @importFrom lattice barchart
#' 
#' @param ffmObj an object of class ffm returned by fitFfm.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL.
#' @param isPlot logical variable to generate plot or not.
#' @param isPrint logical variable to print numeric summary or not.
#' @param stripLeft logical variable to choose the position of strip, 'TRUE' for drawing strips on the left of each panel, 'FALSE' for drawing strips on the top of each panel. Used only when isPlot = 'TRUE'
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display. Used only when isPlot = 'TRUE'
#' @param color  character specifying the plotting color for all the plots
#' @param notch logical. if notch is \code{TRUE}, a notch is drawn in each side of the boxes. If the notches of two plots do not overlap this is strong evidence that the two medians differ (Chambers et al, 1983, p. 62).Default values is \code{FALSE}.
#' @param scaleType scaleType controls if use a same scale of y-axis, choose from c('same', 'free')
#' @param stripText.cex a number indicating the amount by which strip text in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param axis.cex a number indicating the amount by which axis in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param digits digits of printout numeric summary. Used only when isPrint = 'TRUE'
#' @param titleText logical varible to choose display plot title or not. Default is 'TRUE', and used only when isPlot = 'TRUE'.
#' @param which a number to indicate the type of plot. If a subset of the plots 
#' is required, specify a subset of the numbers 1:3 for plots. If \code{which=NULL} (default), the following menu 
#' appears: \cr \cr
#' For plots of a group of assets: \cr
#' 1 = Time series plot of style factor exposures, \cr
#' 2 = Boxplot of style factor exposures, \cr
#' 3 = Barplot of means and vols of style factor exposures, and means of sector exposures (which have no vol). \cr \cr
#' @param type character. type of lattice plot when which=1; 'l' denotes a line, 'p' denotes a point, and 'b' and 'o' both denote both together.deafault is 'b'.
#' @param ... other graphics parameters available in tsPlotMP(time series plot only) can be passed in through the ellipses 
#' 
#' @return  
#' A K x 2 matrix containing mean and standard deviation of K factors
#' 
#' @author Douglas Martin, Lingjie Yi, Avinash
#' @examples 
#'
#' #Load fundamental and return data 
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=as.yearmon("2008-01-01") 
#'           & dat$DATE <= as.yearmon("2012-12-31"),]
#'
#' #Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  
#' 
#' # fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121",
#'               "EP"),date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = TRUE)
#'
#' repExposures(fit.cross, wtsStocks145GmvLo, isPlot = FALSE, digits = 4)
#' repExposures(fit.cross, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, 
#'              which = 2, add.grid = TRUE, scaleType = 'same')
#' repExposures(fit.cross, wtsStocks145GmvLo, isPlot = TRUE, which = 1,
#'              add.grid = FALSE, zeroLine = TRUE, color = 'Blue')
#' repExposures(fit.cross, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, 
#'              which = 3, add.grid = FALSE, zeroLine = FALSE, color = 'Blue')
#' @export


repExposures <- function(ffmObj, weights = NULL, isPlot = TRUE, isPrint = TRUE, scaleType = 'free',
                         stripText.cex =1,axis.cex=1,stripLeft = TRUE, layout = NULL, color = "blue",notch = FALSE, digits = 1, titleText = TRUE, 
                         which = NULL,type="b", ...) {
  
  if (!inherits(ffmObj, "ffm")) {
    stop("Invalid argument: ffmObj should be of class'ffm'.")
  }
  
  which.numeric <- sapply(ffmObj$data[,ffmObj$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- ffmObj$exposure.vars[which.numeric]
  exposures.char <- ffmObj$exposure.vars[!which.numeric]
  
  # get parameter from the factor model fit
  beta = ffmObj$beta
  n.assets = nrow(beta)
  asset.names <- unique(ffmObj$data[[ffmObj$asset.var]])
  TP = length(ffmObj$time.periods)
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }else{
    # check if number of weight parameter matches 
    if(n.assets != length(weights)){
      stop("Invalid argument: incorrect number of weights")
    }
    if(!is.null(names(weights))){
      weights = weights[asset.names]
    }else{
      stop("Invalid argument: names of weights vector should match with asset names")
    }
  } 
  
  X = c()
  for(i in 1:TP){
    
    if(length(exposures.char)>0){
      if(length(exposures.char) == 1){
        dat <- ffmObj$data[ffmObj$data[,ffmObj$date.var]==ffmObj$time.periods[i], ]
        B <- as.matrix(table(dat[,ffmObj$asset.var],dat[,exposures.char]))
        B[B>0] <- 1
        B <- B[asset.names,]
      }else{
        dat <- ffmObj$data[ffmObj$data[,ffmObj$date.var]==ffmObj$time.periods[i], ]
        B <- as.matrix(table(dat[,ffmObj$asset.var],dat[,exposures.char[1]]))
        B[B>0] <- 1
        B <- B[asset.names,]
        for(j in 2:length(exposures.char)){
          temp <- as.matrix(table(dat[,ffmObj$asset.var],dat[,exposures.char[j]]))
          temp[temp>0] <- 1
          temp <- temp[asset.names,]
          B <- cbind(B,temp)
        }
      }
    }else{
      B = c()
    }
    
    dat <- ffmObj$data[ffmObj$data[,ffmObj$date.var]==ffmObj$time.periods[i], ]
    beta <- as.matrix(dat[,exposures.num])
    dimnames(beta) <- list(asset.names, exposures.num)
    beta = cbind(beta,B)
    if(ncol(ffmObj$beta) > ncol(beta)){
      beta = cbind(rep(1,nrow(beta)),beta)
      colnames(beta)[1] = colnames(ffmObj$beta)[1]
    }
    
    temp = as.data.frame(weights %*% beta)
    temp = cbind('Date'=ffmObj$time.periods[i],temp)
    X = rbind(X,temp)
  }
  X = as.xts(X[,-1],order.by = X[,1])
  
  if(isPlot){
    par(mfrow = c(1,1))
    
    which.vec <- which
    which <- which[1]
    
    repeat {
      if (is.null(which)) {
        which <- 
          menu(c("Time series plot of style factor exposures",
                 "Boxplot of style factor exposures",
                 "Barchart of factor exposures"), 
               title="\nMake a plot selection (or 0 to exit):") 
      }
      
      switch(which,
             "1L" = { 
               if(titleText){
                 main = "Style Factor Exposures"
               }else(
                 main = ''
               )
               ## Time Series plot of factor exposures
               tsPlotMP(X[,exposures.num], main = main, stripLeft = stripLeft, layout = layout,color = color,
                        scaleType = scaleType, axis.cex = axis.cex, stripText.cex =stripText.cex,type=type, ...)
             }, 
             "2L" = {
               if(titleText){
                 main = "Distributions of Style Factor Exposures"
               }else(
                 main = ''
               )
               ## Boxplot of factor exposures
               boxplot(100*coredata(X[,exposures.num]), col=color,
                       notch = notch, ylab = "Percentage (%)",
                       main = main)
             }, 
             "3L" = {  

               ## Barplot of means and vols of style factor exposures, and means of sector exposures 

                 a = 100*colMeans(X[,exposures.num])
                 b = 100*apply(X[,exposures.num],2,sd)
                 c = rbind(a,b)
                 sect = as.character(unique(dat[,exposures.char]))
                 d = 100*colMeans(X[,sect])
                if(length(exposures.num)>1)
                {
                  main1 = "Style Exposures Means"
                  main2 = "Style Exposures Volatilities"
                  main3 = "Sector Exposures Means"
                }
                 else if(length(exposures.num)==1)
                 {
                   main1 = "Style Exposures Mean"
                   main2 = "Style Exposures Volatility"
                   main3 = "Sector Exposures Means"
                 }
                dat.StMean = as.data.frame(list("ids" = rep(main1, length(a)), "variable"= names(a), "value"= as.numeric(a)))
                dat.StVol = as.data.frame(list("ids" = rep(main2, length(b)), "variable"= names(b), "value"= as.numeric(b)))
                dat.SecMean = as.data.frame(list("ids" = rep(main3, length(d)), "variable"= names(d), "value"= as.numeric(d)))
                

                plt1 = barchart(value~(variable)|ids,group = (ids),data=dat.StMean,stack =TRUE,layout = layout,col = color,ylab = list(label = "Percentage (%)",cex = axis.cex),
                               scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex, rot = 90)),par.strip.text=list(col="black", cex = stripText.cex))
                plt2 = barchart(value~(variable)|ids,group = (ids),data=dat.StVol,stack =TRUE,layout = layout,col = color,ylab = list(label = "Percentage (%)",cex = axis.cex),
                                scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex, rot = 90)),par.strip.text=list(col="black", cex = stripText.cex))
                plt3 = barchart(value~(variable)|ids,group = (ids),data=dat.SecMean,stack =TRUE,layout = layout,col = color,ylab = list(label = "Percentage (%)",cex = axis.cex),
                                scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex, rot = 90)),par.strip.text=list(col="black", cex = stripText.cex), strip.left = F)
                
                print(plt1, split=c(1,1,2,2), more=TRUE)
                print(plt2, split=c(2,1,2,2), more=TRUE)
                print(plt3, position = c(.25,0,0.75,0.5), more = FALSE)
                
               
             },
             invisible()       
      )        
      # repeat menu if user didn't choose to exit from the plot options
      if (which==0 || length(which.vec)==1) {break} 
      if (length(which.vec)>1) {
        which.vec <- which.vec[-1]
        which <- which.vec[1]
        par(ask=TRUE)
      } else {which=NULL}   
    }
  
  }
    
  
  if(isPrint){
    # tabular report 
    avg = apply(X, 2, mean) * 100
    vol = apply(X, 2, sd) * 100
    stats.sum = cbind(avg, vol)
    colnames(stats.sum) = c('Mean','Volatility')
    
    ret = round(stats.sum, digits)
    
    return(ret)   
  }
}

