#' calculate and display effective style weights
#' 
#' Functions that calculate effective style weights and display the results in
#' a bar chart.  \code{chart.Style} calculates and displays style weights
#' calculated over a single period.  \code{chart.RollingStyle} calculates and
#' displays those weights in rolling windows through time.  \code{style.fit}
#' manages the calculation of the weights by method.  \code{style.QPfit}
#' calculates the specific constraint case that requires quadratic programming.
#' 
#' These functions calculate style weights using an asset class style model as
#' described in detail in Sharpe (1992).  The use of quadratic programming to
#' determine a fund's exposures to the changes in returns of major asset
#' classes is usually refered to as "style analysis".
#' 
#' The "unconstrained" method implements a simple factor model for style
#' analysis, as in: \deqn{Ri = bi1*F1+bi2*F2+...+bin*Fn+ei}{R_i =
#' b_{i1}F_1+b_{i2}F_2+\dots+b_{in}F_n +e_i} where \eqn{Ri}{R_i} represents the
#' return on asset i, \eqn{Fj}{F_j} represents each factor, and \eqn{ei}{e_i}
#' represents the "non-factor" component of the return on i.  This is simply a
#' multiple regression analysis with fund returns as the dependent variable and
#' asset class returns as the independent variables.  The resulting slope
#' coefficients are then interpreted as the fund's historic exposures to asset
#' class returns.  In this case, coefficients do not sum to 1.
#' 
#' The "normalized" method reports the results of a multiple regression
#' analysis similar to the first, but with one constraint: the coefficients are
#' required to add to 1.  Coefficients may be negative, indicating short
#' exposures. To enforce the constraint, coefficients are normalized.
#' 
#' The "constrained" method includes the constraint that the coefficients sum
#' to 1, but adds that the coefficients must lie between 0 and 1. These
#' inequality constraints require a quadratic programming algorithm using
#' \code{\link[quadprog]{solve.QP}} from the 'quadprog' package, and the
#' implementation is discussed under \code{\link{style.QPfit}}.  If set to
#' TRUE, "leverage" allows the sum of the coefficients to exceed 1.
#' 
#' According to Sharpe (1992), the calculation for the constrained case is
#' represented as: \deqn{min var(Rf - sum[wi * R.si]) = min var(F - w*S)}{min
#' \sigma(R_f - \sum{w_i * R_s_i}) = min \sigma(F - w*S)} \deqn{s.t. sum[wi] =
#' 1; wi > 0}{ s.t. \sum{w_i} = 1; w_i > 0}
#' 
#' Remembering that:
#' 
#' \deqn{\sigma(aX + bY) = a^2 \sigma(X) + b^2 \sigma(Y) + 2ab cov(X,Y) =
#' \sigma(R.f) + w'*V*w - 2*w'*cov(R.f,R.s)}
#' 
#' we can drop \eqn{var(Rf)}{\sigma(R_f)} as it isn't a function of weights,
#' multiply both sides by 1/2:
#' 
#' \deqn{= min (1/2) w'*V*w - C'w}{= min (1/2) w'*V*w - C'w} \deqn{ s.t. w'*e =
#' 1, w_i > 0}{ s.t. w'*e = 1, w_i > 0}
#' 
#' Which allows us to use \code{\link[quadprog]{solve.QP}}, which is specified
#' as: \deqn{min(-d' b + 1/2 b' D b)}{min(-d' b + 1/2 b' D b)} and the
#' constraints \deqn{ A' b >= b.0 }{ A' b >= b_0 }
#' 
#' so: b is the weight vector, D is the variance-covariance matrix of the
#' styles d is the covariance vector between the fund and the styles
#' 
#' The chart functions then provide a graphical summary of the results.  The
#' underlying function, \code{\link{style.fit}}, provides the outputs of the
#' analysis and more information about fit, including an R-squared value.
#' 
#' Styles identified in this analysis may be interpreted as an average of
#' potentially changing exposures over the period covered.  The function
#' \code{\link{chart.RollingStyle}} may be useful for examining the behavior of
#' a manager's average exposures to asset classes over time, using a
#' rolling-window analysis.
#' 
#' The chart functions plot a column chart or stacked column chart of the
#' resulting style weights to the current device.  Both \code{style.fit} and
#' \code{style.QPfit} produce a list of data frames containing 'weights' and
#' 'R.squared' results.  If 'model' = TRUE in \code{style.QPfit}, the full
#' result set is shown from the output of \code{solve.QP}.
#' 
#' @aliases chart.Style chart.RollingStyle table.RollingStyle style.fit
#' style.QPfit
#' @param R.fund matrix, data frame, or zoo object with fund returns to be
#' analyzed
#' @param R.style matrix, data frame, or zoo object with style index returns.
#' Data object must be of the same length and time-aligned with R.fund
#' @param method specify the method of calculation of style weights as
#' "constrained", "unconstrained", or "normalized".  For more information, see
#' \code{\link{style.fit}}
#' @param leverage logical, defaults to 'FALSE'.  If 'TRUE', the calculation of
#' weights assumes that leverage may be used.  For more information, see
#' \code{\link{style.fit}}
#' @param model logical. If 'model' = TRUE in \code{\link{style.QPfit}}, the
#' full result set is shown from the output of \code{solve.QP}.
#' @param selection either "none" (default) or "AIC".  If "AIC", then the
#' function uses a stepwise regression to identify find the model with minimum
#' AIC value.  See \code{\link{step}} for more detail.
#' @param unstacked logical.  If set to 'TRUE' \emph{and} only one row of data
#' is submitted in 'w', then the chart creates a normal column chart.  If more
#' than one row is submitted, then this is ignored.  See examples below.
#' @param space the amount of space (as a fraction of the average bar width)
#' left before each bar, as in \code{\link{barplot}}. Default for
#' \code{chart.RollingStyle} is 0; for \code{chart.Style} the default is 0.2.
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param width number of periods or window to apply rolling style analysis
#' over
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param \dots for the charting functions, these are arguments to be passed to
#' \code{\link{barplot}}. These can include further arguments (such as 'axes',
#' 'asp' and 'main') and graphical parameters (see 'par') which are passed to
#' 'plot.window()', 'title()' and 'axis'. For the calculation functions, these
#' are ignored.
#' @note None of the functions \code{chart.Style}, \code{style.fit}, and
#' \code{style.QPfit} make any attempt to align the two input data series. The
#' \code{chart.RollingStyle}, on the other hand, does merge the two series and
#' manages the calculation over common periods.
#' @author Peter Carl
#' @seealso \code{\link{barplot}}, \code{\link{par}}
#' @references Sharpe, W. Asset Allocation: Management Style and Performance
#' Measurement Journal of Portfolio Management, 1992, 7-19.  See \url{
#' http://www.stanford.edu/~wfsharpe/art/sa/sa.htm}
#' @keywords ts multivariate hplot
#' @examples
#' 
#' data(edhec)
#' data(managers)
#' style.fit(managers[97:132,2,drop=FALSE],edhec[85:120,], method="constrained", leverage=FALSE)
#' chart.Style(managers[97:132,2,drop=FALSE],edhec[85:120,], method="constrained", leverage=FALSE, unstack=TRUE, las=3)
#' chart.RollingStyle(managers[,2,drop=FALSE],edhec[,1:11], method="constrained", leverage=FALSE, width=36, cex.legend = .7, colorset=rainbow12equal, las=1)
#' 
`chart.Style` <-
function (R.fund, R.style, method = c("constrained", "unconstrained", "normalized"), leverage = FALSE, main = NULL, ylim = NULL, unstacked=TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # R-Squared could deliver adjusted R-Squared if we wanted

    # FUNCTION:

    # Transform input data to a data frame
    R.fund = checkData(R.fund)
    R.style = checkData(R.style)
    method = method[1]

    # Calculate
    result = style.fit(R.fund, R.style, method = method, leverage = leverage)
    weights = t(as.matrix(result$weights))

    if(is.null(main))
        main = paste(colnames(R.fund)[1] ," Style Weights", sep="")

    if(is.null(ylim))
        if(method == "constrained" & leverage == FALSE) ylim = c(0,1)
        else ylim = NULL

    chart.StackedBar(weights, main = main, ylim = ylim, unstacked = unstacked, ...)
#     barplot(weights, main = main, ylim = ylim, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2008-07-11 03:24:52  peter
# - fixed error with alignment of results
#
# Revision 1.6  2008-04-18 03:58:04  peter
# - reduced to a wrapper to chart.StackedBar
#
# Revision 1.5  2008/02/27 04:05:32  peter
# - added 'leverage' tag to eliminate sum to one constraint
# - added cex.names for controlling size of xaxis labels
#
# Revision 1.4  2008/02/26 04:49:06  peter
# - handles single column fits better
#
# Revision 1.3  2008/02/26 04:39:40  peter
# - moved legend and margin control into chart.StackedBar
# - handles multiple columns
#
# Revision 1.2  2008/02/23 05:35:56  peter
# - set ylim more sensibly depending on method
#
# Revision 1.1  2008/02/23 05:32:37  peter
# - simple bar chart of a fund's exposures to a set of factors, as determined
# by style.fit
#
#
###############################################################################
