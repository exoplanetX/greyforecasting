#'
#' @export

LSE <- function(y,...){
  Y=matrix(y,ncol=1)
  V=list(...)
  n=length(V)
  m=length(y)
  if(n>2){
    u=LSEn(y,V)
    return(u)
  }
  B=matrix(ncol = n,nrow = m)
  B[,1]=V[[1]]
  if(n>=2){
    for(i in 2:n){
      B[,i]=V[[i]]
    }
  }
  u=as.vector(solve(t(B)%*%B)%*%t(B)%*%Y)

  names(u)=c('a','b')
  return(u)
}
