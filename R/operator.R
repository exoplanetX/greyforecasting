#' classical buffer operator
#' @export


operator<- function(x,alpha=0.5){
  if(!(alpha<=1&&alpha>=0)) {
    alpha<-0.5
    warning("parameter alpha out of possible range")
  }
  n=length(x)
  y=rep(x[n],n)
  names(y)=names(x)
  for(i in seq_along(x)){
    y[i]<-alpha*x[i]+(1-alpha)*x[n]
  }
  return(y)
}

