#' basic grey incidence analysis formula
#'
basicGI <- function(x0,x,e=0.5){
  n=length(x0)
  c=length(x)
  gi=c()
  m=c()
  M=c()
  x0=oimage(x0)
  for(i in 1:c) x[[i]]=oimage(x[[i]])
  gamma=matrix(nrow=c,ncol=n)
  for(i in 1:c){
    m[i]=min(abs(x0-x[[i]]))
    M[i]=max(abs(x0-x[[i]]))
  }
  mm=min(m)
  MM=max(M)
  for(i in 1:c){
    for(k in 1:n){
      gamma[i,k]=(mm+e*MM)/(abs(x0[k]-x[[i]][k])+e*MM)
    }
    gi[i]=sum(gamma[i,])/n
  }
  return(gi)
}
