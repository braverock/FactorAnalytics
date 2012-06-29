rCornishFisher <-
function(n, sigma, skew, ekurt, seed=NULL) {
## inputs:
## n          scalar, number of simulated values
## sigma      scalar, standard deviation
## skew       scalar, skewness
## ekurt      scalar, excess kurtosis
## outputs:
## n simulated values from Cornish-Fisher distribution
if (!is.null(seed)) set.seed(seed)
zc = rnorm(n)
z.cf = zc  + (((zc^2 - 1) * skew)/6) + (((zc^3 - 3 * zc) *
      ekurt)/24) - ((((2 * zc^3) - 5 * zc) * skew^2)/36)
ans = sigma*z.cf
ans
}

