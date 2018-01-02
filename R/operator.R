#' classical buffer operator
#'

operator<- function(y,r=0.5){
  n<-length(y)
  if(r<=0) r<-1
  for(i in 1:r){
    y<-r*y+(1-r)*y[n]
  }
  y
}



