#' @title Time Series Plots
#' 
#' @description Plot time series with specific plotting parameters 
#'
#' @importForm lattice panel.abline
#' 
#' @param ret an time series exposure/return object  
#' @param stripLeft logical variable to choose the position of strip, "TRUE" for drawing strips on the left of each panel, "FALSE" for drawing strips on the top of each panel
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display.
#' @param scaleType scaleType controls if use a same scale of y-axis, choose from c('same', 'free')
#' @param add.grid logical varible.If 'TRUE', type = c('l', 'g'); If 'FALSE', type = c('l')
#' @param zeroLine logical varible to choose add a dotted horizontal line at the zero vertical distance
#' 
#' @author Douglas Martin, Lingjie Yi
#' @examples 
#'
#' #Load the data 
#' data("stocks145scores6")
#' dat = stocks145scores6
#' returns = tapply(dat$RETURN,list(dat$DATE,dat$TICKER),I)
#' ret = xts(returns[,1:5],as.yearmon(rownames(returns)))
#' 
#' #generate return time series plot               
#' tsPlotMP(ret, color = 'Blue')
#' tsPlotMP(ret, scaleType = "same", zeroLine = F)
#' tsPlotMP(ret, stripLeft = F, main = 'Time Series Plot')
#'  
#'    
#' @export

# Lattice type time series plotting function
tsPlotMP = function(ret,add.grid = F,cex = 1.0, layout = NULL,type = "l", 
                    pct = 100, yname = "RETURNS (%)",scaleType = "free", stripLeft = T,
                    main = NULL, lwd = 1, color = "black", zeroLine = T)
{
  strip.left = stripLeft
  strip = !strip.left
  if(add.grid) {type = c("l","g")} else
  {type = type}
  
  if(zeroLine){
    panel =  function(...){
      panel.abline(h=0,lty = 3)
      panel.xyplot(...)
    }
  }else{
    panel =  function(...){
      panel.xyplot(...)
    }
  }

  pl = xyplot(pct*ret,par.strip.text = list(cex = cex),type = type,
              xlab="", ylab = list(label = yname,cex = cex), lwd = lwd,
              scales = list(y = list(cex = cex,relation=scaleType),
                            x = list(cex = cex)),layout = layout,main = main,
              col = color, strip = strip, strip.left = strip.left,
              panel =  panel)
  
  print(pl)
}


