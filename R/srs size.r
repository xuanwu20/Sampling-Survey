size.mean=function(N=NULL, Mean.his=NULL, Var.his, method, bound, alpha)
{
  quan=qnorm(1-alpha/2)
  
  if (method=="V") {n0=Var.his/bound}
  if (method=="CV"){n0=Var.his/(bound*Mean.his)^2}
  if (method=="d") {n0=(quan/bound)^2*Var.his}
  if (method=="r") {n0=(quan/(bound*Mean.his))^2*Var.his}
  
  size=ifelse(is.null(N), n0, n0/(1+n0/N))
  return(list(method=method, n0=round(n0), size=round(size)))
}


size.total=function(N=NULL, Mean.his=NULL, Var.his, method, bound, alpha)
{
  quan=qnorm(1-alpha/2)
  
  if (method=="V") {n0=N^2*Var.his/bound}
  if (method=="CV"){n0=Var.his/(bound*Mean.his)^2}
  if (method=="d") {n0=(N*quan/bound)^2*Var.his}
  if (method=="r") {n0=(quan/(bound*Mean.his))^2*Var.his}
  
  size=ifelse(is.null(N), n0, n0/(1+n0/N))
  return(list(method=method, n0=round(n0), size=round(size)))
}


size.prop=function(N=NULL, Prop.his, method, bound, alpha)
{
  quan=qnorm(1-alpha/2)
  
  if (method=="V") {n0=Prop.his*(1-Prop.his)/bound}
  if (method=="CV"){n0=(1-Prop.his)/(bound^2*Prop.his)}
  if (method=="d") {n0=(quan^2)*Prop.his*(1-Prop.his)/(bound^2)}
  if (method=="r") {n0=(quan^2)*(1-Prop.his)/(bound^2*Prop.his)}
  
  size=ifelse(is.null(N), n0, n0/(1+(n0-1)/N))
  return(list(method=method, n0=round(n0), size=round(size)))
}


size.num=function(N, Prop.his, method, bound, alpha)
{
  quan=qnorm(1-alpha/2)
  
  if (method=="V") {n0=N^2*Prop.his*(1-Prop.his)/bound}
  if (method=="CV"){n0=N^2*(1-Prop.his)/(bound^2*Prop.his)}
  if (method=="d") {n0=N^4*Prop.his*(1-Prop.his)*(quan/boud)^2}
  if (method=="r") {n0=N^2*(quan/bound)^2*(1-Prop.his)/Prop.his}
  
  size=n0/(1+(n0-1)/N)
  return(list(method=method, n0=round(n0), size=round(size)))
}
  