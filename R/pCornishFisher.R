pCornishFisher <-
function(q,n,skew, ekurt) {
zq = q 
CDF = pnorm(zq)  +   1/sqrt(n) *(skew/6 * (1-zq^2))*dnorm(zq) + 
     1/n *( (ekurt)/24*(3*zq-zq^3)+ (skew)^2/72*(10*zq^3 - 15*zq -zq^5))*dnorm(zq)
return(CDF)
}

