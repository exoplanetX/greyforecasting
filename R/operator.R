#' classical buffer operator
#'

operator<- function(y,r){
  n<-length(y)
  if(r<=0) r<-1
  for(i in 1:r){
    y<-(y+y[n])/2
  }
  y
}


