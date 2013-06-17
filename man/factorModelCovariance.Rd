\name{factorModelCovariance}
\alias{factorModelCovariance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Factor Model Covariance Matrix.
}
\description{
Compute asset return covariance matrix from factor model parameters.
}
\usage{
factorModelCovariance(beta.mat, factor.cov, residVars.vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta.mat}{
\code{N x K} matrix of factor betas, where \code{N} is the number of assets and
\code{K} is the number of factors.
}
  \item{factor.cov}{
\code{K x K} factor return covariance matrix.
}
  \item{residVars.vec}{
\code{N x 1} vector of asset specific residual variances from the factor model.
}
}
\details{
The return on asset \code{i} (\code{i = 1,...,N}) is assumed to follow the factor model \cr
\code{R(i,t) = alpha + t(beta)*F(t) + e(i,t), e(i,t) ~ iid (0, sig(i)^2)} \cr
where \code{beta} is a \code{K x 1} vector of factor exposures. The return variance
is then \cr
\code{var(R(i,t) = t(beta)*var(F(t))*beta + sig(i)^2}, \cr
and the \code{N x N} covariance matrix of the return vector \code{R} is \cr
\code{var(R) = B*var(F(t))*t(B) + D} \cr
where B is the \code{N x K} matrix of asset betas and \code{D} is a diagonal matrix with
\code{sig(i)^2} values along the diagonal.
}
\value{ 
\code{N x N} return covariance matrix based on factor model parameters.
}
\references{
Zivot, E. and J. Wang (2006), \emph{Modeling Financial Time Series with S-PLUS, 
Second Edition}, Springer-Verlag.
}
\author{
Eric Zivot and Yi-An Chen. 
}
\examples{
# factorModelCovariance
data(managers.df)
factors    = managers.df[,(7:9)]
ret.assets = managers.df[,(1:6)]
fit <-fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                  variable.selection="all subsets", factor.set = 3)
factorModelCovariance(fit$beta.mat,var(factors),fit$residVars.vec)
}
