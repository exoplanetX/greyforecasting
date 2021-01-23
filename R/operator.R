#' classical buffer operator
#' @export


operator<- function(y,alpha=0.5,is.obj=FALSE){
  if(is.na(alpha)){
    alpha<-0.5
  }else{
    if(!(alpha<=1&&alpha>=0)) {
    alpha<-0.5
    warning("parameter alpha out of possible range")
    }
  }
  n<-length(y)
#####obj or process################
  if(is.obj==TRUE){
    obj<-bo.obj(y)
    if(alpha<=0) alpha<-1
    obj.formula <- function(y,alpha) {
      for(i in seq_along(y)){
      y[i]<-alpha*y[i]+(1-alpha)*y[n]
      }
    }
    obj.expression <- substitute(y^(1)(k+1)==(1-a)*x(k-1)+a*x(k) ,list(a=alpha) ) #equation
    obj <- bo.obj(y,formula=obj.formula,expression=obj.expression)
    return(obj)
  }else{
    for(i in seq_along(y)){
      y[i]<-alpha*y[i]+(1-alpha)*y[n]
    }
    return(y)
  }


}
