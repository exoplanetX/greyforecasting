#'
#' @export

LSEn <- function(y,...){
  Y=matrix(y,ncol=1)
  V=list(...)
  n=length(V)
  m=length(y)
  if(n==2){
    u=LSE(y,V[[1]],V[[2]])
    return(u)
  }
  B=matrix(ncol = n,nrow = m)
  B[,1]=V[[1]]
  if(n>=2){
    for(i in 2:n){
      B[,i]=V[[i]]
      if(is.list(V[[i]])){
        nd=length(V[[i]])
        for(j in 1:nd){
          B[,i+j]=V[[i]][[j]]
        }
      }
    }
  }
  u=as.vector(solve(t(B)%*%B)%*%t(B)%*%Y)
  nd+2
  names(u)=letters[1:(nd+2)]
  return(u)
}
