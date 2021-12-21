#' @title portWeights
#'
#' @description Creates portfolio weights for a specified portfolio. In
#' this version it just computes a global minimum variance long-only
#' portfolio (gmvLO).
#' 
#' @details This function use the PortfolioAnalytics, ROI,
#' and ROI.plugin.quadprog packages
#' 
#' @importFrom PortfolioAnalytics portfolio.spec add.constraint add.objective optimize.portfolio
#' @importFrom ROI Q_objective
#' 
#' @param returns A K-dimensional xts time series of returns
#'
#' @return
#' A K-dimensional xts time series of portfolio weights
#' 
#' @author Doug Martin
#' 
#' @references
#' Martin et al. (2022) Portfolio Construction and Risk Analysis, Chapter 4.
#'
#' @examples
#' library(FactorAnalytics)
#' library(xts)
#' data(stocksCRSP)
#' data(factorsSPGMI)
#' stocks <- selectCRSPandSPGMI(stocksCRSP,factorsSPGMI)
#' returnMat = tapply(stocks[["Return"]],list(stocks$Date, stocks$Ticker), I)
#' returns = xts(returnMat, as.yearmon(rownames(returnMat)))
#' wtsGmvLO <- portWeights(returns)
#' @export
portWeights <- function(returns)
{
  #library(PortfolioAnalytics)
  #library(ROI)
  #library(ROI.plugin.quadprog)
  funds <- colnames(returns)
  pspec.base <- PortfolioAnalytics::portfolio.spec(funds)
  pspec.fi <- PortfolioAnalytics::add.constraint(portfolio = pspec.base, 
                                                 type="full_investment")
  pspec.uc <- PortfolioAnalytics::add.objective(portfolio = pspec.fi,
                                                type = "risk",
                                                name = "var")
  pspec.lo <- PortfolioAnalytics::add.constraint(portfolio = pspec.uc,
                                                 type="long_only")
  opt.lo <- PortfolioAnalytics::optimize.portfolio(returns, 
                                                   pspec.lo, 
                                                   optimize_method = "quadprog")
  wtsGmvLO <- round(opt.lo$weights,digits = 4)
  
  wtsGmvLO
}