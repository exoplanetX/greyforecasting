
operator<- function(y,r){
  n<-length(y)
  if(r<=0) r<-1
  for(i in 1:r){
    y<-(y+y[n])/2
  }
  y
}
svwbo<-function(y,alpha){
  for(i in rev( seq_along(y[1:(length(y)-1)]) ) ){
    y[i]<-alpha*y[i]+(1-alpha)*y[i+1]
  }
  y
}
