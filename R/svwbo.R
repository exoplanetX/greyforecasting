#' smooth buffer operator with variable weight
#'
#' smooth buffer operator with variable weight
#' @param y original data
#' @param alpha weight,alpha=0.5
#' @examples format:y<-svwbo(y,alpha=0.5)
#' @export
svwbo<-function(x,alpha=0.5){
  if(is.na(alpha)){
    alpha<-0.5
  }else{
    if(!(alpha<=1&&alpha>=0)) {
      alpha<-0.5
      warning("parameter alpha out of possible range")
    }
  }
  n=length(x)
  y=rep(x[n],n)
  names(y)=names(x)
  for(i in rev( seq_along(x[1:(length(y)-1)]) ) ){
    y[i]<-alpha*x[i]+(1-alpha)*x[i+1]
  }
  return(y)
}
