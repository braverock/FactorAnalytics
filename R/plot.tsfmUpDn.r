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
#' @importFrom xts merge.xts
#' @importFrom zoo coredata
#' @param x an object of class \code{tsfmUpDn} produced by \code{fitTsfmUpDn}.
#' @param asset.name A vector of character to show single or multiple assets names. The defualt if 
#' \code{NULL}.  
#' @param line.color A vector of color codes of up/dn fitted line. The first element is for the object fitted
#' line and the second for the comparison fitted line. The default is \code{c("blue","purple")}.
#' @param line.type A vector of line types of up/dn fitted line. The first is for the object fitted
#' line and the second for the comparison fitted line. The default is \code{c("dashed","solid"}.
#' @param line.width A vector of line width of up/dn fitted line. The first element is for the object fitted
#' line and the second element for the comparison fitted line. The default is \code{c(1,2}.
#' @param add.legend A logic flag to add a legend. The default is \code{TRUE}.
#' @param SFM.line A logic flag to add a fitted single factor model. The default is \code{FALSE}.
#' @param sfm.line.type SFM line type. The default is \code{"dashed"}
#' @param LSandRob A logic flag to add a comparison Up/Down factor model. If the original model
#' is \code{"LS"}, the comparison model is \code{"Robust"} and vice versa. The default is \code{FALSE}.
#' The default is \code{FALSE}.  
#' @param legend.loc The default is \code{"topleft"}.
#' @param legend.cex \code{cex} of \code{legend}.
#' @param ... Other arguments can be used in \code{plot}. Please refer to \code{plot}.
#' @author Yi-An Chen
#' 
#' @seealso \code{\link{fitTsfmUpDn}} 
#' 
#' @examples
#' 
#' # load data from the database
#'  data(managers, package = 'PerformanceAnalytics')
#'  colnames(managers) = make.names(colnames(managers))
#' # example: Up and down market factor model with  fit
#'  fitUpDn <- fitTsfmUpDn(asset.names=colnames(managers[,(1:6)]),
#'  mkt.name="SP500.TR", data=managers, fit.method="LS")
#' # plot the fitted model of every assets, press enter to show the next plot.
#'  plot(fitUpDn)
#'  
#' # or choose to plot one specific asset
#'  plot(fitUpDn,asset.name="HAM1")
#'  
#' # add a single market factor model fitted line
#'  plot(fitUpDn,SFM.line=TRUE,asset.name="HAM1")
#'              
#' # add Robust Up/Dn model fitted line and change legend to 
#' # show the robust up/dn Beta                               
#'  plot(fitUpDn,LSandRob=TRUE,asset.name="HAM1")
#'  
#'                                                                                                                                      
#' @method plot tsfmUpDn
#' @export


plot.tsfmUpDn <- function(x,asset.name=NULL,SFM.line=FALSE,LSandRob=FALSE,
                          line.color=c("blue","purple"),line.type=c("dashed","solid"),
                          line.width=c(1,2),sfm.line.type = "dashed",
                          add.legend=TRUE,legend.loc="topleft",legend.cex=0.9,...) {
  
  # specify the name of market returns and the assets returns
  mkt.name = x$Up$factor.names
  
  # add SFM estimation 
  
  if (SFM.line) {
    data = x$data
    asset.names = x$Up$asset.names
    fit.method = x$Up$fit.method
    fitSf <-  fitTsfm(asset.names=asset.names,factor.names=mkt.name,mkt.name=mkt.name,rf.name=NULL,
                      data=data,fit.method=fit.method)
    plotDataSf <- merge.xts(fitted(fitSf),fitSf$data[,mkt.name])
  }
  
  # add LS/Robust Up/Dn comparison
  
  if (LSandRob) {
    fit.methods <- c("LS","Robust")
    x$call$fit.method <- fit.methods[!fit.methods%in%x$Up$fit.method]
    x.alt <- eval(x$call)  
  }
  
  if (is.null(asset.name)) { 
    assets.name.all = x$Up$asset.names    
  } else {
    assets.name.all = asset.name  
  }
    while(length(assets.name.all)>0){
      assets.name = assets.name.all[1]
      # extract info from the fitTsfm x
      plotDataUp <- merge.xts(x$Up$data[,c(assets.name,mkt.name)], fitted(x$Up)[,assets.name])
      colnames(plotDataUp) <- c("ActualUp","MktUp","FittedUp")
      plotDataDn <-merge.xts(x$Dn$data[,c(assets.name,mkt.name)], fitted(x$Dn)[,assets.name])
      colnames(plotDataDn) <- c("ActualDn","MktDn","FittedDn")
    
      plot(rbind(zoo::coredata(plotDataUp$MktUp),zoo::coredata(plotDataDn$MktDn)),
           rbind(zoo::coredata(plotDataUp$ActualUp),zoo::coredata(plotDataDn$ActualDn)),
           xlab=mkt.name,ylab=assets.name,...)
      abline(v=0)
      lines(zoo::coredata(plotDataUp$MktUp),zoo::coredata(plotDataUp$FittedUp),col=line.color[1],lty=line.type[1],lwd=line.width[1])
      lines(zoo::coredata(plotDataDn$MktDn),zoo::coredata(plotDataDn$FittedDn),col=line.color[1],lty=line.type[1],lwd=line.width[1])
      abline(h=0)
      
      up.beta <- round(summary(x$Up)$sum.list[[assets.name]]$coefficients[mkt.name,1:2],2)
      dn.beta <- round(summary(x$Dn)$sum.list[[assets.name]]$coefficients[mkt.name,1:2],2)
      up.beta <- paste(as.character(up.beta)[1]," (",as.character(up.beta)[2],")",sep="")
      dn.beta <- paste(as.character(dn.beta)[1]," (",as.character(dn.beta)[2],")",sep="")
    
      # add LS line 
      if (SFM.line){
        lines(zoo::coredata(plotDataSf[,mkt.name]),zoo::coredata(plotDataSf[,assets.name]),lty=sfm.line.type)
       # legend.name = paste(fit.method,"fitted line",seq="")
        
      }
      
      # add alternative Up/Dn model for comparison
      if (LSandRob){
        plotDataUp.alt <- merge.xts(x.alt$Up$data[,c(assets.name,mkt.name)], fitted(x.alt$Up)[,assets.name])
        colnames(plotDataUp.alt) <- c("ActualUp","MktUp","FittedUp")
        plotDataDn.alt <-merge.xts(x.alt$Dn$data[,c(assets.name,mkt.name)], fitted(x.alt$Dn)[,assets.name])
        colnames(plotDataDn.alt) <- c("ActualDn","MktDn","FittedDn")
        lines(zoo::coredata(plotDataUp.alt$MktUp),zoo::coredata(plotDataUp.alt$FittedUp),col=line.color[2],lty=line.type[2],lwd=line.width[2])
        lines(zoo::coredata(plotDataDn.alt$MktDn),zoo::coredata(plotDataDn.alt$FittedDn),col=line.color[2],lty=line.type[2],lwd=line.width[2])
        
       
        up.beta.alt <- round(summary(x.alt$Up)$sum.list[[assets.name]]$coefficients[mkt.name,1:2],2)
        dn.beta.alt <- round(summary(x.alt$Dn)$sum.list[[assets.name]]$coefficients[mkt.name,1:2],2)
        up.beta.alt <- paste(as.character(up.beta.alt)[1]," (",as.character(up.beta.alt)[2],")",sep="")
        dn.beta.alt <- paste(as.character(dn.beta.alt)[1]," (",as.character(dn.beta.alt)[2],")",sep="")
                
      }
      
      if (add.legend){
        
        if (LSandRob){
            if (x$call$fit.method=="Robust") {
                beta.legend = c(paste("Up Beta","      =",up.beta,seq=""),
                                paste("Up BetaRob =",up.beta.alt,seq=""),
                                paste("Dn Beta","      =",dn.beta,seq=""),
                                paste("Dn BetaRob =",dn.beta.alt,seq=""))
            } else {
                beta.legend = c("Up BetaRob","Up Beta","Dn BetaRob","Dn Beta")
                beta.legend = c(paste("Up BetaRob =",up.beta.alt,seq=""),
                                paste("Up Beta","      =",up.beta,seq=""),
                                paste("Dn BetaRob =",dn.beta.alt,seq=""),
                                paste("Dn Beta","      =",dn.beta,seq=""))
                                
            }
          legend(legend.loc,legend=beta.legend,ncol=1,cex=legend.cex,bty="n",lty=rep(line.type,2),col=rep(line.color,2))
        } else {
            if (x$Up$fit.method=="Robust") {
            beta.legend = c(paste("Up BetaRob =",up.beta.alt,seq=""),
                            paste("Dn BetaRob =",dn.beta.alt,seq=""))
          } else {
            beta.legend = c("Up Beta","Dn Beta")
            beta.legend = c(paste("Up Beta =",up.beta,seq=""),
                            paste("Dn Beta =",dn.beta,seq=""))
          }
                                   
          legend(legend.loc,legend=beta.legend,ncol=1,cex=legend.cex,bty="n")
        }
        # legend.lty = c(line.type,NA,NA,line.type,NA,NA) 
        #  legend.col = c(line.col,NA,NA,line.col,NA,NA)
         # legend(legend.loc,legend=legend.txt,ncol=2,lty=legend.lty,col=legend.col,cex=legend.cex)            
                
      }
      assets.name.all <- assets.name.all[-1]
      par(ask=TRUE)
    }
  # turn it back
  par(ask=FALSE)
  }