#' @name factorDataSetDjia
#' @title DJIA stocks Compustat factors 14yrs
#' @description Contains returns for 30 DJIA stocks spanned across 9 Sectors -ENERGY, COSTAP, INDUS,T MATRLS,
#'               FINS, INFOTK, HEALTH, CODISC, and TELCOM stocks along with 4 factor data (MKTCAP, ENTVAL, P2B, EV2S, SIZE)
#'              starting from Jan 2000 to march 2013.
#'              
#'              The 9 Sectors correspond to Energy,ConsumerStaples, Industrials, Materials, Financials,
#'              InformationTechnology, HealthCare, ConsumerDiscretionary and Telecommunications respectively.
#' @docType data
#' @source TBA
#' @usage data("factorDataSetDjia")
NULL

#' @name factorDataSetDjia5Yrs
#' @title DJIA stocks Compustat factors 5yrs
#' @description  Contains returns for 30 DJIA stocks spanned across 9 Sectors -ENERGY, COSTAP, INDUS,T MATRLS,
#'               FINS, INFOTK, HEALTH, CODISC, and TELCOM stocks along with 4 factor data (MKTCAP, ENTVAL, P2B, EV2S, SIZE)
#'               starting from  from Jan 2008 to Dec 2012.
#'               
#'               The 9 Sectors correspond to Energy, ConsumerStaples, Industrials, Materials, Financials,
#'               InformationTechnology, HealthCare, ConsumerDiscretionary and Telecommunications respectively.
#' @docType data
#' @source TBA
#' @usage data("factorDataSetDjia5Yrs")
NULL

#' @name stocks145scores6
#' @title CRSP stocks Capital IQ scores
#' @description Contains returns for 145 stocks starting from Jan 1990 to Dec 2014 spanned across 10 Sectors-
#'              ENERGY, COSTAP, INDUS,T MATRLS, FINS, INFOTK, HEALTH, CODISC, UTILS and TELCOM 
#'              along with 6 factors: ROE, BP, MOM121, SIZE, VOL121, EP
#'   
#'              The 10 Sectors correspond to Energy, ConsumerStaples, Industrials, Materials, Financials,
#'              InformationTechnology, HealthCare, ConsumerDiscretionary,
#'              Utilities and Telecommunications respectively.
#'              
#' @docType data
#' @source TBA
#' @usage data("stocks145scores6")
#' 
#' 
NULL

#' @name wtsDjiaGmv
#' @title DJIA GMV portfolio weights
#' @description Contains weights obtained after optimizing the portfolio returns of the 30 DJIA stocks (from dataset factorDataSetDjia5Yrs) 
#'              for a global minimum variance portfolio starting from Jan 2008 to Dec 2012.
#' @docType data
#' @source TBA
#' @usage data("wtsDjiaGmv")
NULL

#' @name wtsDjiaGmvLo
#' @title DJIA GMV long-only portfolio weights
#' @description Contains weights obtained after optimizing the portfolio returns of the 30 DJIA stocks (from dataset factorDataSetDjia5Yrs) 
#'              for a long-only global minimum variance portfolio starting from Jan 2008 to Dec 2012.
#' @docType data
#' @source TBA
#' @usage data("wtsDjiaGmvLo")
NULL

#' @name wtsStocks145Gmv
#' @title CRSP 145 stocks GMV portfolio weights
#' @description Contains weights obtained after optimizing the portfolio returns of 145 stocks (from dataset stocks145scores6) 
#'              for a global minimum variance portfolio starting from Jan 1990 to Dec 2014.
#' @docType data
#' @source TBA
#' @usage data("wtsStocks145Gmv")
NULL

#' @name wtsStocks145GmvLo
#' @title CRSP 145 stocks GMV long-only weights
#' @description Contains weights obtained after optimizing the portfolio returns of 145 stocks (from dataset stocks145scores6) 
#'              for a long-only global minimum variance portfolio starting from Jan 1990 to Dec 2014.
#' @docType data
#' @source TBA
#' @usage data("wtsStocks145GmvLo")
NULL

#' @name managers
#' @title time-series data
#' @description Hypothetical Alternative Asset Manager and Benchmark Data for Time Series Factor Model Fit
#' @docType data
#' @source TBA
#' @usage data("managers")
NULL

#' @name managers.ffm
#' @title managers data for ffm
#' @description Hypothetical Alternative Asset Manager and Benchmark Data for Time Series Factor Model Fit
#' @docType data
#' @source TBA
#' @usage data("managers.ffm")
NULL