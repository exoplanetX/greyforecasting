#' AGO operator
#'
#' Accumulation operating generation
#' @export
#' @param x sequential data
#' @param r order of accumulation operating generation.

ago<- function(x,r=1){
  n=length(x)
  nm=names(x)
  A=matrix(0,nrow=n,ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      if(i==j)  A[i,j]=1
      if(i>j) A[i,j]=choose(i-j+r-1,i-j)
    }
  }
  rs=as.vector(A%*%x)
  names(rs)=nm
  return(rs)

}
