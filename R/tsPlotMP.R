#' @title Plot
#' 
#' @description 
#' 
#' @export

#Not the final version 


# Lattice type time series plotting function
tsPlotMP = function(ret,add.grid = F,cex = 1.0, layout = NULL,type = "l",
                    pct = 100, yname = "RETURNS (%)",scaleType = "free",
                    stripLeft = T,main = NULL,
                    lwd = 1, color = "black")
{
  strip.left = stripLeft
  strip = !strip.left
  if(add.grid) {type = c("l","g")} else
  {type = type}
  
  pl = xyplot(pct*ret,par.strip.text = list(cex = cex),type = type,
         xlab="", ylab = list(label = yname,cex = cex), lwd = lwd,
         scales = list(y = list(cex = cex,relation=scaleType),
                       x = list(cex = cex)),layout = layout,main = main,
                       col = color, strip = strip, strip.left = strip.left)
  
  print(pl)
}


