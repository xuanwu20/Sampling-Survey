ratio=function(y.sample, x.sample, N=NULL, auxiliary=FALSE, Xbar=NULL, alpha)
{
  n=length(y.sample)
  f=ifelse(is.null(N), 0, n/N)
  nf=(1-f)/n

  ybar=mean(y.sample)
  xbar=mean(x.sample)
  
  sy2 =var(y.sample)
  sx2 =var(x.sample)
  syx =cov(y.sample, x.sample)
  
  cy2 =nf*sy2/(ybar^2)
  cx2 =nf*sx2/(xbar^2)
  cyx =nf*syx/(ybar*xbar)
  
  ratio.est=ybar/xbar
  
  if(auxiliary==FALSE)
  {
    ratio.var=(nf/xbar^2)*(sy2+ratio.est^2*sx2-2*ratio.est*syx)
    ratio.sd =sqrt(ratio.var)
  }
  else
  {
    ratio.var=(nf/Xbar^2)*(sy2+ratio.est^2*sx2-2*ratio.est*syx)
    ratio.sd =sqrt(ratio.var)
  }  
  
  
  ### CI: Method 1 ####################################
  ci1=conf.interval(ratio.est, ratio.sd, alpha)
  left1 =ci1$left
  right1=ci1$right
  
  
  ### CI: Method 2 ####################################
  quan=qnorm(1-alpha/2)
  
  est2=1-quan^2*cyx
  var2=(cy2+cx2-2*cyx)-quan^2*(cy2*cx2-cyx^2)
  
  ci2=conf.interval(est2, sqrt(var2), alpha)
  left2 =ratio.est*(ci2$left)/(1-quan^2*cx2)
  right2=ratio.est*(ci2$right)/(1-quan^2*cx2)
  
  ### CI: Method 3 ####################################
  var3=ratio.est^2*(cy2+cx2-2*cyx)
  
  ci3=conf.interval(ratio.est, sqrt(var3), alpha)
  left3 =ci3$left
  right3=ci3$right
  
  ratio.ci=t(cbind(c(left1, right1), c(left2, right2), c(left3, right3)))
  colnames(ratio.ci)=c("Left", "Right")
  rownames(ratio.ci)=c("Classic", "Exact", "Exact2")
  return(list(ratio.est=ratio.est, ratio.var=ratio.var, ratio.sd=ratio.sd, ratio.ci=as.data.frame(ratio.ci)))
}


