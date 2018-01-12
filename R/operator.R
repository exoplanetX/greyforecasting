#' classical buffer operator
#'
bo.obj <- function(x,formula=NULL,expression=NULL){
  obj <-list(
    data=x,
    formula=formula,
    expression=expression
  )
  class(obj) <- "bo"
  obj
}

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

#' smooth buffer operator with variable weight
#' y:original data
#' alpha:weight
#' format:y<-svwbo(y,alpha=0.5)
#' defaut alpha=0.5
svwbo<-function(y,alpha=0.5){
  if(is.na(alpha)){
    alpha<-0.5
  }else{
    if(!(alpha<=1&&alpha>=0)) {
      alpha<-0.5
      warning("parameter alpha out of possible range")
    }
  }
  for(i in rev( seq_along(y[1:(length(y)-1)]) ) ){
    y[i]<-alpha*y[i]+(1-alpha)*y[i+1]
  }
  y
}

