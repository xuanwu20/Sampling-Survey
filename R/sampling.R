######################### single sampling ########################
srs.sampling=function(N, n, full.data){
  set.seed(020)
  srs.subset=sample(1:N, n)
  srs.sample=full.data[srs.subset,]
  return(srs.sample)
}

####################### stratified sampling ######################
stra.sampling=function(nh, data.stra){
  len=length(nh)
  stra.sample=Sample=NULL
  
  for(h in 1:len)
  {
    stra.hth=data.stra[[h]]
    N.hth=nrow(stra.hth)
    n.hth=nh[h]
    
    stra.sample[[h]]=srs.sampling(N.hth, n.hth, stra.hth)
    rownames(stra.sample[[h]])=c(1:nrow(stra.sample[[h]]))
    
    Sample=rbind(Sample, stra.sample[[h]])
  }
  return(Sample)
}