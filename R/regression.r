regression.mean=function(y.sample, x.sample, N=NULL, Xbar, alpha, method="Min", beta0=NULL)
{
  n=length(y.sample)
  f=ifelse(is.null(N), 0, n/N)
  nf=(1-f)/n
  
  ybar=mean(y.sample)
  xbar=mean(x.sample)
  
  sy2 =var(y.sample)
  sx2 =var(x.sample)
  syx =cov(y.sample, x.sample)
  
  if (method=="Min")
  {
    beta=syx/sx2
    ybar.reg.est=ybar+beta*(Xbar-xbar)
    ybar.reg.var=nf*(n-1)/(n-2)*(sy2-syx^2/sx2)
    ybar.reg.sd =sqrt(ybar.reg.var)
    
    ci=conf.interval(ybar.reg.est, ybar.reg.sd, alpha)
    left =ci$left
    right=ci$right
    
    ybar.reg.result=matrix(c(ybar.reg.est, ybar.reg.var, ybar.reg.sd, left, right), nrow=1)
    colnames(ybar.reg.result)=c("Est", "Var", "SD", "Left", "Right")
    rownames(ybar.reg.result)=c("Mean_Reg")
  }
  
  if (method=="Constant")
  {
    beta=beta0
    ybar.reg.est=ybar+beta*(Xbar-xbar)
    ybar.reg.var=nf*(sy2+beta^2*sx2-2*beta*syx)
    ybar.reg.sd =sqrt(ybar.reg.var)
    
    ci=conf.interval(ybar.reg.est, ybar.reg.sd, alpha)
    left =ci$left
    right=ci$right
    
    ybar.reg.result=matrix(c(ybar.reg.est, ybar.reg.var, ybar.reg.sd, left, right), nrow=1)
    colnames(ybar.reg.result)=c("Est", "Var", "SD", "Left", "Right")
    rownames(ybar.reg.result)=c("Mean_Reg")
  }
  return(ybar.reg.result=as.data.frame(ybar.reg.result))
}


regression.total=function(y.sample, x.sample, N=NULL, Xbar, alpha, method="Min", beta0=NULL)
{
  ybar.reg.result=regression.mean(y.sample, x.sample, N, Xbar, alpha, method, beta0)
  ybar.reg.est  =ybar.reg.result$Est
  ybar.reg.var  =ybar.reg.result$Var
  ybar.reg.left =ybar.reg.result$Left
  ybar.reg.right=ybar.reg.result$Right
  
  ytotal.reg.est  =N*ybar.reg.est              
  ytotal.reg.var  =N^2*ybar.reg.var
  ytotal.reg.sd   =sqrt(ytotal.reg.var)
  ytotal.reg.left =N*ybar.reg.left
  ytotal.reg.right=N*ybar.reg.right
  
  ytotal.reg.result=matrix(c(ytotal.reg.est, ytotal.reg.var, ytotal.reg.sd, ytotal.reg.left, ytotal.reg.right), nrow=1)
  colnames(ytotal.reg.result)=c("Est", "Var", "SD", "Left", "Right")
  rownames(ytotal.reg.result)=c("Total_Reg")
  
  return(ytotal.reg.result=as.data.frame(ytotal.reg.result))
}


