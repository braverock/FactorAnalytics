#' @title Plot actual against fitted values of up and down market time series factor model
#' 
#' @description Generic \code{plot} method for object of class \code{tsfmUpDn}.
#' 
#' @details 
#' This method plots actual values against fitted value of up and down market time series
#' factor model. The dots are actual values and the dashed lines are fitted values. Users can 
#' choose to add a single market factor model and a robust up and down model for comaprsion. 
#' 
#' For other types of plots, use the list objects \code{Up} and \code{Dn} of class \code{tsfmUpDn}. 
#' The \code{plot.tsfm} can be applied.
#' 
#' @param object an object of class \code{tsfmUpDn} produced by \code{fitTsfmUpDn}.
#' @param which.assets A vector of character to show single or multiple assets names. The defualt if 
#' \code{NULL}.  
#' @param line.color A vector of color codes of up/dn fitted line. The first color is for the object fitted
#' line and the second color for the comparison fitted line. The default is \code{c("blue","purple")}.
#' @param line.type The line type of up/dn fitted values. The default is \code{"dotted"}.
#' @param add.legend A logic flag to add a legend. The default is \code{TRUE}.
#' @param add.SFM.line A logic flag to add a fitted single factor model. The default is \code{FALSE}.
#' @param add.comparison A logic flag to add a comparison Up/Down factor model. If the original model
#' is \code{"OLS"}, the comparison model is \code{"Robust"} and vice versa. The default is \code{FALSE}.
#' @param show.comparison.legend A logic flag to opt for showing the legend of the comparison model. 
#' The default is \code{FALSE}.  
#' @param legend.loc The default is \code{"topleft"}.
#' @param legend.cex \code{cex} of \code{legend}.
#' 
#' @param ... Other arguments can be used in \code{plot}. 
#' @author Yi-An Chen
#' 
#' @seealso \code{\link{fitTsfmUpDn}} 
#' 
#' @examples
#' 
#' # load data from the database
#'  data(managers)
#' # example: Up and down market factor model with OLS fit
#'  fitUpDn <- fitTsfmUpDn(asset.names=colnames(managers[,(1:6)]),mkt.name="SP500.TR",
#'                        data=managers, fit.method="OLS")
#' # plot the fitted model of every assets, press enter to show the next plot.
#'  plot(fitUpDn)
#'  
#' # or choose to plot one specific asset
#'  plot(fitUpDn,which.assets="HAM1")
#'  
#' # add a single market factor model fitted line
#'  plot(fitUpDn,add.SFM.line=TRUE,which.assets="HAM1")
#'              
#' # add Robust Up/Dn model fitted line and change legend to show the robust up/dn Beta                               
#'  plot(fitUpDn,add.comparison=TRUE,show.comparison.legend=TRUE,add.SFM.line=TRUE,which.assets="HAM1")
#'  
#'                                                                                                                                      
#' @method plot tsfmUpDn
#' @export


plot.tsfmUpDn <- function(object,which.assets=NULL,add.SFM.line=FALSE,add.comparison=FALSE,
                          line.color=c("blue","purple"),line.type="dotted",
                          add.legend=TRUE,legend.loc="topleft",legend.cex=0.9,
                          show.comparison.legend=FALSE,...) {
  
  # specify the name of market returns and the assets returns
  mkt.name = object$Up$factor.names
  
  # add SFM estimation 
  
  if (add.SFM.line) {
    data = object$data
    asset.names = object$Up$asset.names
    fit.method = object$Up$fit.method
    fitSf <-  fitTsfm(asset.names=asset.names,factor.names=mkt.name,mkt.name=mkt.name,rf.name=NULL,
                      data=data,fit.method=fit.method)
    plotDataSf <- merge.xts(fitted(fitSf),fitSf$data[,mkt.name])
  }
  
  # add LS/Robust Up/Dn comparison
  
  if (add.comparison) {
    fit.methods <- c("OLS","Robust")
    object$call$fit.method <- fit.methods[!fit.methods%in%object$call$fit.method]
    object.alt <- eval(object$call)  
  }
  
  
  if (is.null(which.assets)) { 
    assets.name.all = object$Up$asset.names    
  } else {
    assets.name.all = which.assets  
  }
    while(length(assets.name.all)>0){
      assets.name = assets.name.all[1]
      # extract info from the fitTsfm object
      plotDataUp <- merge.xts(object$Up$data[,c(assets.name,mkt.name)], fitted(object$Up)[,assets.name])
      colnames(plotDataUp) <- c("ActualUp","MktUp","FittedUp")
      plotDataDn <-merge.xts(object$Dn$data[,c(assets.name,mkt.name)], fitted(object$Dn)[,assets.name])
      colnames(plotDataDn) <- c("ActualDn","MktDn","FittedDn")
    
      plot(rbind(coredata(plotDataUp$MktUp),coredata(plotDataDn$MktDn)),
           rbind(coredata(plotDataUp$ActualUp),coredata(plotDataDn$ActualDn)),
           xlab=mkt.name,ylab=assets.name,...)
      abline(v=0)
      lines(coredata(plotDataUp$MktUp),coredata(plotDataUp$FittedUp),col=line.color[1],lty=line.type)
      lines(coredata(plotDataDn$MktDn),coredata(plotDataDn$FittedDn),col=line.color[1],lty=line.type)
      abline(h=0)
      
      up.beta <- round(summary(object$Up)$sum.list[[assets.name]]$coefficients[mkt.name,1:2],2)
      dn.beta <- round(summary(object$Dn)$sum.list[[assets.name]]$coefficients[mkt.name,1:2],2)
      up.beta <- c(as.character(up.beta)[1],paste("(",as.character(up.beta)[2],")",sep=""))
      dn.beta <- c(as.character(dn.beta)[1],paste("(",as.character(dn.beta)[2],")",sep=""))
      line.col = line.color[1]
      # add LS line 
      if (add.SFM.line){
        lines(coredata(plotDataSf[,assets.name]),coredata(plotDataSf[,mkt.name]),lty="dotted")
        legend.name = paste(fit.method,"fitted line",seq="")
        
      }
      
      # add alternative Up/Dn model for comparison
      if (add.comparison){
        plotDataUp.alt <- merge.xts(object.alt$Up$data[,c(assets.name,mkt.name)], fitted(object.alt$Up)[,assets.name])
        colnames(plotDataUp.alt) <- c("ActualUp","MktUp","FittedUp")
        plotDataDn.alt <-merge.xts(object.alt$Dn$data[,c(assets.name,mkt.name)], fitted(object.alt$Dn)[,assets.name])
        colnames(plotDataDn.alt) <- c("ActualDn","MktDn","FittedDn")
        lines(coredata(plotDataUp.alt$MktUp),coredata(plotDataUp.alt$FittedUp),col=line.color[2],lty=line.type)
        lines(coredata(plotDataDn.alt$MktDn),coredata(plotDataDn.alt$FittedDn),col=line.color[2],lty=line.type)
        
        # add comparison legend 
        if (show.comparison.legend) {
        up.beta <- round(summary(object.alt$Up)$sum.list[[assets.name]]$coefficients[mkt.name,1:2],2)
        dn.beta <- round(summary(object.alt$Dn)$sum.list[[assets.name]]$coefficients[mkt.name,1:2],2)
        up.beta <- c(as.character(up.beta)[1],paste("(",as.character(up.beta)[2],")",sep=""))
        dn.beta <- c(as.character(dn.beta)[1],paste("(",as.character(dn.beta)[2],")",sep=""))
        line.col = line.color[2]
        } 
      }
      
      
      
      if (add.legend){
          legend.txt = c("Up Beta",up.beta,"Dn Beta",dn.beta)                          
          legend.lty = c(line.type,NA,NA,line.type,NA,NA) 
          legend.col = c(line.col,NA,NA,line.col,NA,NA)
          legend(legend.loc,legend=legend.txt,ncol=2,lty=legend.lty,col=legend.col,cex=legend.cex)            
                
      }
      assets.name.all <- assets.name.all[-1]
      par(ask=TRUE)
    }
  # turn it back
  par(ask=FALSE)
  }