# Yi-An Chen
# July 5, 2012

factorModelAttribution <- 
  function(fit) {
    class = class(fit,benchmark)
  # input
 # class:   Class has to be either MacroFactorModel, FundmentalFactorModel
 #          or StatFactorModel 
 # benchmark: benchmark returns, default is           
    # only class of 3 fit model can be used
  if (class !="MacroFactorModel" && class !="FundmentalFactorModel"
      && class != "StatFactorModel")
    {
    stop("Class has to be either MacroFactorModel, FundmentalFactorModel
         or StatFactorModel")
  }
    
    
  # beginning of switching  
    switch(class,
    MacroFactorModel={
     
      
    },       
    FundmentalFactorModel={
      print("test 2")
    },
    StatFactorModel={
      
    }       
           
           )
  }