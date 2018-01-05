#' classical buffer operator
#'

operator<- function(y,alpha=0.5){
  n<-length(y)
  if(alpha<=0) alpha<-1
  for(i in seq_along(y)){
    y[i]<-alpha*y[i]+(1-alpha)*y[n]
  }
  y
}

#' smooth buffer operator with variable weight
#' y:original data
#' alpha:weight
#' format:y<-svwbo(y,alpha=0.5)
#' defaut alpha=0.5
svwbo<-function(y,alpha=0.5){
  for(i in rev( seq_along(y[1:(length(y)-1)]) ) ){
    y[i]<-alpha*y[i]+(1-alpha)*y[i+1]
  }
  y
}

