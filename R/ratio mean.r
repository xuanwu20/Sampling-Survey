ratio.mean=function(y.sample, x.sample, N=NULL, Xbar, alpha)
{ 
  ratio.result=ratio(y.sample, x.sample, N, auxiliary=TRUE, Xbar, alpha)
  ratio.est=ratio.result$ratio.est
  ratio.var=ratio.result$ratio.var
  ratio.ci =ratio.result$ratio.ci
  
  ybarR.est=Xbar*ratio.est              
  ybarR.var=Xbar^2*ratio.var
  ybarR.sd =sqrt(ybarR.var)
  ybarR.ci =Xbar*ratio.ci
  
  return(list(ybarR.est=ybarR.est, ybarR.var=ybarR.var, ybarR.sd=ybarR.sd, 
              ybarR.ci=as.data.frame(ybarR.ci)))
}
