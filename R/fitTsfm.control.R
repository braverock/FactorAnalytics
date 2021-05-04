#' @title List of control parameters for \code{fitTsfm}
#' 
#' @description Creates a list of control parameters for \code{\link{fitTsfm}}. 
#' All control parameters that are not passed to this function are set to 
#' default values. This function is meant for internal use only!! 
#' 
#' @details This control function is used to process optional arguments passed 
#' via \code{...} to \code{fitTsfm}. These arguments are validated and defaults
#' are set if necessary before being passed internally to one of the following
#' functions: \code{\link[stats]{lm}}, \code{\link[RobStatTM]{lmrobdetMM}}, 
#' \code{\link[stats]{step}}, \code{\link[leaps]{regsubsets}}, 
#' \code{\link[lars]{lars}} and \code{\link[lars]{cv.lars}}. See their 
#' respective help files for more details. The arguments to each of these 
#' functions are listed above in approximately the same order for user 
#' convenience.
#' 
#' The scalar \code{decay} is used by \code{\link{fitTsfm}} to compute 
#' exponentially decaying weights for \code{fit.method="DLS"}. Alternately, one 
#' can directly specify \code{weights}, a weights vector, to be used with 
#' "LS" or "Robust". Especially when fitting multiple assets, care should be 
#' taken to ensure that the length of the weights vector matches the number of
#' observations (excluding cases ignored due to NAs).
#' 
#' \code{lars.criterion} selects the criterion (one of "Cp" or "cv") to 
#' determine the best fitted model for \code{variable.selection="lars"}. The 
#' "Cp" statistic (defined in page 17 of Efron et al. (2004)) is calculated 
#' using \code{\link[lars]{summary.lars}}. While, "cv" computes the K-fold 
#' cross-validated mean squared prediction error using 
#' \code{\link[lars]{cv.lars}}.
#' 
#' @param decay a scalar in (0, 1] to specify the decay factor for "DLS". 
#' Default is 0.95.
#' @param weights an optional vector of weights to be used in the fitting 
#' process for \code{fit.method="LS","Robust"}, or 
#' \code{variable.selection="subsets"}. Should be \code{NULL} or a numeric 
#' vector. The length of \code{weights} must be the same as the number of 
#' observations. The weights must be nonnegative and it is strongly 
#' recommended that they be strictly positive.
#' @param model,x,y,qr logicals passed to \code{lm} for 
#' \code{fit.method="LS"}. If \code{TRUE} the corresponding components of the 
#' fit (the model frame, the model matrix, the response, the QR decomposition) 
#' are returned.
#' @param nrep the number of random subsamples to be drawn for 
#' \code{fit.method="Robust"}. If the data set is small and "Exhaustive" 
#' resampling is being used, the value of \code{nrep} is ignored.
#' @param tuning.chi tuning constant for the function used to compute the M-scale
#' used for the initial S-estimator. If missing, it is computed inside \code{lmrobdet.control} to match
#' the value of \code{bb} according to the family of rho functions specified in \code{family}.
#' @param bb tuning constant (between 0 and 1/2) for the M-scale used to compute the initial S-estimator. It
#' determines the robustness (breakdown point) of the resulting MM-estimator, which is
#' \code{bb}. Defaults to 0.5.
#' @param tuning.psi tuning parameters for the regression M-estimator computed with a rho function
#' as specified with argument \code{family}. If missing, it is computed inside \code{lmrobdet.control} to match
#' the value of \code{efficiency} according to the family of rho functions specified in \code{family}.
#' Appropriate values for \code{tuning.psi} for a given desired efficiency for Gaussian errors
#' can be constructed using the functions \link{bisquare}, \link{modopt} and \link{optimal}.
#' @param efficiency desired asymptotic efficiency of the final regression M-estimator. Defaults to 0.85.
#' @param max.it maximum number of IRWLS iterations for the MM-estimator
#' @param refine.tol relative convergence tolerance for the S-estimator
#' @param rel.tol relative convergence tolerance for the IRWLS iterations for the MM-estimator
#' @param refine.PY number of refinement steps for the Pen~a-Yohai candidates
#' @param solve.tol relative tolerance for inversion
#' @param trace.lev positive values (increasingly) provide details on the progress of the MM-algorithm
#' @param compute.rd logical value indicating whether robust leverage distances need to be computed.
#' @param family string specifying the name of the family of loss function to be used (current valid
#' options are "bisquare", "optimal" and "modopt"). Incomplete entries will be matched to
#' the current valid options.
#' @param corr.b logical value indicating whether a finite-sample correction should be applied
#' to the M-scale parameter \code{bb}.
#' @param split.type determines how categorical and continuous variables are split. See
#' \code{\link[robustbase]{splitFrame}}.
#' @param initial string specifying the initial value for the M-step of the MM-estimator. Valid
#' options are \code{'S'}, for an S-estimator and \code{'MS'} for an M-S estimator which is
#' appropriate when there are categorical explanatory variables in the model.
#' @param psc_keep For \code{pyinit}, proportion of observations to remove based on PSCs. The effective proportion of removed
#' observations is adjusted according to the sample size to be \code{prosac*(1-p/n)}. See \code{\link{pyinit}}.
#' @param resid_keep_method For \code{pyinit}, how to clean the data based on large residuals. If
#' \code{"threshold"}, all observations with scaled residuals larger than \code{C.res} will
#' be removed, if \code{"proportion"}, observations with the largest \code{prop} residuals will
#' be removed. See \code{\link{pyinit}}.
#' @param resid_keep_thresh See parameter \code{resid_keep_method} above. See \code{\link{pyinit}}.
#' @param resid_keep_prop See parameter \code{resid_keep_method} above. See \code{\link{pyinit}}.
#' @param py_maxit Maximum number of iterations. See \code{\link{pyinit}}.
#' @param py_eps Relative tolerance for convergence.  See \code{\link{pyinit}}.
#' @param mscale_maxit Maximum number of iterations for the M-scale algorithm. See \code{\link{pyinit}}.
#' @param mscale_tol Convergence tolerance for the M-scale algorithm. See \code{\link{pyinit}}.
#' @param mscale_rho_fun String indicating the loss function used for the M-scale. See \code{\link{pyinit}}.
#' @param scope defines the range of models examined in the \code{"stepwise"} 
#' search. This should be either a single formula, or a list containing 
#' components \code{upper} and \code{lower}, both formulae. See 
#' \code{\link[stats]{step}} for how to specify the formulae and usage.
#' @param scale optional parameter for \code{variable.selection="stepwise"}. 
#' The argument is passed to \code{\link[stats]{step}} or 
#' \code{\link[RobStatTM]{step.lmrobdetMM}} as appropriate.
#' @param direction the mode of \code{"stepwise"} search, can be one of "both", 
#' "backward", or "forward", with a default of "both". If the \code{scope} 
#' argument is missing the default for \code{direction} is "backward". 
#' @param trace If positive (or, not \code{FALSE}), info is printed during the 
#' running of  \code{\link[stats]{step}}, \code{\link[lars]{lars}} or 
#' \code{\link[lars]{cv.lars}} as relevant. Larger values may give more 
#' detailed information. Default is \code{FALSE}.
#' @param steps the maximum number of steps to be considered for 
#' \code{"stepwise"}. Default is 1000 (essentially as many as required). It is 
#' typically used to stop the process early. 
#' @param k the multiple of the number of degrees of freedom used for the 
#' penalty in \code{"stepwise"}. Only \code{k = 2} gives the genuine AIC. 
#' \code{k = log(n)} is sometimes referred to as BIC or SBC. Default is 2.
#' @param nvmin minimum size of subsets to examine for \code{"subsets"}. 
#' Default is 1.
#' @param nvmax maximum size of subsets to examine for \code{"subsets"}. 
#' Default is 8.
#' @param force.in index to columns of design matrix that should be in all 
#' models for \code{"subsets"}. Default is \code{NULL}.
#' @param force.out index to columns of design matrix that should be in no 
#' models for \code{"subsets"}. Default is \code{NULL}.
#' @param method one of "exhaustive", "forward", "backward" or "seqrep" 
#' (sequential replacement) to specify the type of subset search/selection. 
#' Required if \code{variable selection="subsets"} is chosen. Default is 
#' "exhaustive".
#' @param really.big option for \code{"subsets"}; Must be \code{TRUE} to 
#' perform exhaustive search on more than 50 variables.
#' @param type option for \code{"lars"}. One of "lasso", "lar", 
#' "forward.stagewise" or "stepwise". The names can be abbreviated to any 
#' unique substring. Default is "lasso".
#' @param normalize option for \code{"lars"}. If \code{TRUE}, each variable is 
#' standardized to have unit L2 norm, otherwise they are left alone. Default 
#' is \code{TRUE}.
#' @param eps option for \code{"lars"}; An effective zero.
#' @param max.steps Limit the number of steps taken for \code{"lars"}; the 
#' default is \code{8 * min(m, n-intercept)}, with \code{m} the number of 
#' variables, and \code{n} the number of samples. For \code{type="lar"} or 
#' \code{type="stepwise"}, the maximum number of steps is 
#' \code{min(m,n-intercept)}. For \code{type="lasso"} and especially 
#' \code{type="forward.stagewise"}, there can be many more terms, because 
#' although no more than \code{min(m,n-intercept)} variables can be active 
#' during any step, variables are frequently droppped and added as the 
#' algorithm proceeds. Although the default usually guarantees that the 
#' algorithm has proceeded to the saturated fit, users should check.
#' @param plot.it option to plot the output for \code{\link[lars]{cv.lars}}. 
#' Default is \code{FALSE}.
#' @param lars.criterion an option to assess model selection for the 
#' \code{"lars"} method; one of "Cp" or "cv". See details. Default is "Cp".
#' @param K number of folds for computing the K-fold cross-validated mean 
#' squared prediction error for \code{"lars"}. Default is 10.
#' 
#' @return A list of the above components. This is only meant to be used by 
#' \code{fitTsfm}.
#' 
#' @author Sangeetha Srinivasan
#' 
#' @references 
#' Efron, B., Hastie, T., Johnstone, I., & Tibshirani, R. (2004). Least angle 
#' regression. The Annals of statistics, 32(2), 407-499. 
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link[stats]{lm}}, 
#' \code{\link[RobStatTM]{lmrobdetMM}}, \code{\link[stats]{step}}, 
#' \code{\link[leaps]{regsubsets}}, \code{\link[lars]{lars}} and 
#' \code{\link[lars]{cv.lars}}
#' 
#' @examples
#' \dontrun{
#' # check argument list passed by fitTsfm.control
#' tsfm.ctrl <- fitTsfm.control(method="exhaustive", nvmin=2)
#' print(tsfm.ctrl)
#' }
#' 
#' # used internally by fitTsfm in the example below
#' data(managers)
#' fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                factor.names=colnames(managers[,(7:9)]), 
#'                data=managers, variable.selection="subsets", 
#'                method="exhaustive", nvmin=2)
#'
#' @export



fitTsfm.control <- function(
		decay=0.95, 
		weights, 
		model=TRUE, 
		x=FALSE, 
		y=FALSE, 
		qr=TRUE, 
		nrep=NULL,
		### from lmrobdet.control
		bb = 0.5, 
		efficiency = 0.95, 
		family = "mopt",
		tuning.psi, 
		tuning.chi, 
		compute.rd = FALSE, 
		corr.b = TRUE,
		split.type = "f", 
		initial = "S", 
		max.it = 100,
		refine.tol = 1e-07, 
		rel.tol = 1e-07, 
		refine.PY = 10,
		solve.tol = 1e-07, 
		trace.lev = 0, 
		psc_keep = 0.5,
		resid_keep_method = "threshold", 
		resid_keep_thresh = 2,
		resid_keep_prop = 0.2, 
		py_maxit = 20, 
		py_eps = 1e-05,
		mscale_maxit = 50, 
		mscale_tol = 1e-06, 
		mscale_rho_fun = "bisquare",
		### end lmrobdet.control
		scope, 
		scale, 
		direction,  
		steps=1000, 
		k=2, 
		nvmin=1, 
		nvmax=8, 
		force.in=NULL, 
		force.out=NULL, 
		method, 
		really.big=FALSE, 
		type, 
		normalize=TRUE, 
		eps=.Machine$double.eps, 
		max.steps, 
		plot.it=FALSE, 
		lars.criterion="Cp", 
		K=10) {
	
	# get the user-specified arguments (that have no defaults)
	# this part of the code was adapted from stats::lm
	call <- match.call()
	m <- match(c("weights","scope","scale","direction","method","type",
					"max.steps"), names(call), 0L) 
	
	# drop unused levels
	if (!is.null(call) && sum(m>0) == 0) {
		args <- list()
	} else {
		args <- as.list(call[m, drop=TRUE])
	}
	
	# check input validity for some of the arguments
	if (decay<=0 || decay>1) {
		stop("Invalid args: Decay factor should be in (0,1]")
	}
	if (!is.logical(model) || length(model) != 1) {
		stop("Invalid args: control parameter 'model' must be logical")
	}
	if (!is.logical(x) || length(x) != 1) {
		stop("Invalid args: control parameter 'x' must be logical")
	}
	if (!is.logical(y) || length(y) != 1) {
		stop("Invalid args: control parameter 'y' must be logical")
	}
	if (!is.logical(qr) || length(qr) != 1) {
		stop("Invalid args: control parameter 'qr' must be logical")
	}
	if (!is.logical(really.big) || length(really.big) != 1) {
		stop("Invalid args: control parameter 'really.big' must be logical")
	}
	if (!is.logical(normalize) || length(normalize) != 1) {
		stop("Invalid args: control parameter 'normalize' must be logical")
	}
	if (!is.logical(plot.it) || length(plot.it) != 1) {
		stop("Invalid args: control parameter 'plot.it' must be logical")
	}
	if (nvmin <= 0 || round(nvmin) != nvmin) {
		stop("Control parameter 'nvmin' must be a positive integer")
	}
	if (nvmax < nvmin || nvmin < length(force.in)) {
		stop("Invaid args: nvmax should be >= nvmin and nvmin 
						should be >= length(force.in)")
	}
	if (!is.logical(normalize) || length(normalize) != 1) {
		stop("Invalid args: control parameter 'normalize' must be logical")
	}
	if (!(lars.criterion %in% c("Cp","cv"))) {
		stop("Invalid args: lars.criterion must be 'Cp' or 'cv'.")
	}
	
	# return list of arguments with defaults if they are unspecified
	result <- c(args, list(
					decay=decay, 
					model=model, 
					x=x, 
					y=y, 
					qr=qr, 
					nrep=nrep, 
					bb = bb,
					efficiency=efficiency,
					family = family,
					compute.rd = compute.rd,
					corr.b = corr.b,
					split.type = split.type,
					initial=initial,
					max.it = max.it, 
					refine.tol = refine.tol, 
					rel.tol = rel.tol,
					refine.PY = refine.PY,
					solve.tol = solve.tol, 
					trace.lev = trace.lev,
					psc_keep = psc_keep, 
					resid_keep_method = resid_keep_method,
					resid_keep_thresh = resid_keep_thresh, 
					resid_keep_prop = resid_keep_prop, 
					py_maxit = py_maxit, 
					py_eps = py_eps,
					mscale_maxit = mscale_maxit, 
					mscale_tol = mscale_tol, 
					mscale_rho_fun = mscale_rho_fun,
					steps=steps, 
					k=k, 
					nvmin=nvmin, 
					nvmax=nvmax, 
					force.in=force.in, 
					force.out=force.out, 
					really.big=really.big, 
					normalize=normalize, 
					eps=eps, 
					plot.it=plot.it, 
					lars.criterion=lars.criterion, 
					K=K))
	return(result)
}
