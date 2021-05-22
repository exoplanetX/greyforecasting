#' absolute grey incidence
#'
abGI <- function(x0,x){
#  x0=xx$x1
#  x=y
  n=length(x0)
  c=length(x)
  xz=zimage(x0)
  s=c()
  sd=c()
  kesi=c()
  for(i in 1:c) x[[i]]=zimage(x[[i]])
  s0=abs(sum(xz[2:(n-1)])+0.5*xz[n])
  for(i in 1:c){
    s[i]=abs(sum(x[[i]][2:(n-1)])+0.5*x[[i]][n])
    sd[i]=abs( sum(x[[i]][2:(n-1)]-xz[2:(n-1)]) + 0.5*(x[[i]][n]-xz[n]) )
    kesi[i]=(1+s0+s[i])/(1+s0+s[i]+sd[i])
  }
  return(kesi)
}
