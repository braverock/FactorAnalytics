dCornishFisher <-
function(x, n,skew, ekurt) {

density <- dnorm(x) + 1/sqrt(n)*(skew/6*(x^3-3*x))*dnorm(x) +
    1/n *( (skew)^2/72*(x^6 - 15*x^4 + 45*x^2 -15) + ekurt/24 *(x^4-6*x^2+3) )*dnorm(x)
return(density)
}

