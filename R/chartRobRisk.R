#' @title \code{riskBudget} Plots
#'
#' @description Plot the elements of an \code{riskBudget} object.
#'
#' @importFrom lattice barchart
#'
#' @param object An object of class \code{riskBudget} returned by \code{robRiskBudget}.
#' @param digits The number of digits of numerical values in graphs
#' @param col The vector of numerics representing the color of the initial and final risk budgetings, respectively. 

#' @param ... other graphics parameters in plot
#'
#' @return The graph of initial risk budgets and final risk budgets
#'
#' @author Chindhanai Uthaisaad
#'
#' @examples
#' data("RussellData")
#' rf = RussellData[, 16]
#' robRiskData = RussellData[, 1:15]
#'
#' riskBudget = robRiskBudget(robRiskData, rf = rf, shrink = TRUE, avgCor = TRUE,
#' ESMethod = "historical", corMatMethod = "mcd")
#' chartRobRisk(riskBudget)
#'
#' @export

chartRobRisk = function(object, digits = 3, col = c(4, 2), ...) {

  options(digits = digits)

  # Plot of initial and final risk budgets
  xmax = max(max(object$initialRiskBudget), max(object$finalRiskBudget))
  xmin = min(min(object$initialRiskBudget), min(object$finalRiskBudget))
  P1 = barchart(object$initialRiskBudget, col = col[1], origin = 0,
                           xlim = c(xmin - 5e-4, xmax + 5e-4),
                           xlab = "Initial Risk Budgets",
                           main = "Risk Budgets Plot")
  P2 = barchart(object$finalRiskBudget, col = col[2], origin = 0,
                           xlim = c(xmin - 5e-4, xmax + 5e-4),
                           xlab = "Final Risk Budget")
  print(P1, split=c(1,1,1,2), more=TRUE)
  print(P2, split=c(1,2,1,2))

}


