# Yi-An Chen
# July 5, 2012

factorModelRiskAttribution <- 
  function(fit) {
    class = class(fit,benchmark,start,end,Full.sample=TRUE)
  # input
 # class:   Class has to be either MacroFactorModel, FundmentalFactorModel
 #          or StatFactorModel 
 # benchmark: benchmark returns, default is equally weighted portfolio   
 # start :  Start of trailling period. 
 # end   :  End of trailling period.  
 # Full.sample: Is full sample included in the analysis. Default is TRUE.   
  
 # output
 # class of "FMattribution" 
 #    
 # plot.FMattribution     
 # summary.FMattribution    
 # print.FMattribution    
  if (class !="MacroFactorModel" && class !="FundmentalFactorModel"
      && class != "StatFactorModel")
    {
    stop("Class has to be either MacroFactorModel, FundmentalFactorModel
         or StatFactorModel")
  }
    # get portfolio names and factor names
    manager.names = colnames(fit.macro$ret.assets)
    factor.names  = colnames(fit.macro$factors)
    
  # beginning of switching  
    switch(class,
    MacroFactorModel={ 
      for (i in manager.names) {
      
      total.ret = fit$ret.assets
      total.sd = sd(total)
      active.ret = benchmark - fit$ret.assets
      active.sd = ad(active)
      expected.active.ret = beta.mat%*%fit.macro$factors - benchmark
      exceptional.active.ret = active- expected.active.ret
      Market.timing
      Risk.indexes
      Industries
      Asset.selection
      Trading
      Transaction.cost
      }
        
        },       
    FundmentalFactorModel={
      print("test 2")
    },
    StatFactorModel={
      
    }       
           
           )
  }