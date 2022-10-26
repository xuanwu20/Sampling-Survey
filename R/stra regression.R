separate.reg.mean=function(Nh, y.sample, x.sample, stra.index, Xbarh, alpha, method="Min", Beta0=NULL)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  
  nh=numeric(stra.num)
  syh2=sxh2=syxh=numeric(stra.num)
  xhbar=yhbar=ylrSh.reg.est=numeric(stra.num)

  for(h in 1:stra.num)
  {
    y.hth=y.sample[stra.index==h]
    x.hth=x.sample[stra.index==h]
    yhbar[h]=mean(y.hth)
    xhbar[h]=mean(x.hth)
    
    nh[h]=length(y.hth)
    
    syh2[h] =var(y.hth)
    sxh2[h] =var(x.hth)
    syxh[h] =cov(y.hth, x.hth)
  }
  
  n=sum(nh)
  fh=nh/Nh
  nf=(1-fh)/nh
  
  if (method=="Min"){
    beta=syxh/sxh2
    
    ylrSh.reg.est=yhbar+beta*(Xbarh-xhbar)
    
    ylrS.est=sum(Wh*ylrSh.reg.est)
    ylrS.var=sum(Wh^2*nf*(n-1)/(n-2)*(syh2-syxh^2/sxh2))
    ylrS.sd =sqrt(ylrS.var)
    
    ylrS.ci=conf.interval(ylrS.est, ylrS.sd, alpha)
    ylrS.left =ylrS.ci$left
    ylrS.right=ylrS.ci$right
    
    ylrS.result=matrix(c(ylrS.est, ylrS.var, ylrS.sd, ylrS.left, ylrS.right), nrow=1)
    colnames(ylrS.result)=c("Est", "Var", "SD", "Left", "Right")
    rownames(ylrS.result)="Mean_lrS"
  }
  
  if (method=="Constant"){
    beta=Beta0
    
    ylrSh.reg.est=yhbar+beta*(Xbarh-xhbar)
    
    ylrS.est=sum(Wh*ylrSh.reg.est)
    ylrS.var=sum(Wh^2*nf*(syh2+beta^2*sxh2-2*beta*syxh))
    ylrS.sd =sqrt(ylrS.var)
    
    ylrS.ci=conf.interval(ylrS.est, ylrS.sd, alpha)
    ylrS.left =ylrS.ci$left
    ylrS.right=ylrS.ci$right
    
    ylrS.result=matrix(c(ylrS.est, ylrS.var, ylrS.sd, ylrS.left, ylrS.right), nrow=1)
    colnames(ylrS.result)=c("Est", "Var", "SD", "Left", "Right")
    rownames(ylrS.result)="Mean_lrS"
  }
  
  return(ylrS.result=as.data.frame(ylrS.result))
}




combined.reg.mean=function(Nh, y.sample, x.sample, stra.index, Xbar, alpha, method="Min", beta0=NULL)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  
  nh=numeric(stra.num)
  syh2=sxh2=syxh=numeric(stra.num)
  
  yst.result=stra.srs.mean2(Nh, y.sample, stra.index, alpha)
  yst.est=yst.result[[2]]$mean.est
  
  xst.result=stra.srs.mean2(Nh, x.sample, stra.index, alpha)
  xst.est=xst.result[[2]]$mean.est
  
  for (h in 1:stra.num)
  {
    y.hth=y.sample[stra.index==h]
    x.hth=x.sample[stra.index==h]
    
    nh[h]=length(y.hth)
    
    syh2[h] =var(y.hth)
    sxh2[h] =var(x.hth)
    syxh[h] =cov(y.hth, x.hth)
  }
  
  fh=nh/Nh
  nf=(1-fh)/nh
  
  if(method=="Min"){beta=sum(Wh^2*nf*syxh)/sum(Wh^2*nf*sxh2)}
  if(method=="Constant"){beta=beta0}
  
  ylrC.est=yst.est+beta*(Xbar-xst.est)
  ylrC.var=sum(Wh^2*nf*(syh2+beta^2*sxh2-2*beta*syxh))
  ylrC.sd =sqrt(ylrC.var)
  
  ylrC.ci=conf.interval(ylrC.est, ylrC.sd, alpha)
  ylrC.left =ylrC.ci$left
  ylrC.right=ylrC.ci$right
  
  ylrC.result=matrix(c(ylrC.est, ylrC.var, ylrC.sd, ylrC.left, ylrC.right), nrow=1)
  colnames(ylrC.result)=c("Est", "Var", "SD", "Left", "Right")
  rownames(ylrC.result)="Mean_lrC"
  return(ylrC.result=as.data.frame(ylrC.result))
}


