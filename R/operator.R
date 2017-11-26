#' 算数平均缓冲算子，经典缓冲算子
#'

operator<- function(y,r){
  n<-length(y)
  if(r<=0) r<-1
  for(i in 1:r){
    y<-(y+y[n])/2
  }
  y
}


