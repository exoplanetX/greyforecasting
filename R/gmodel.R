# not use
gmodel<-function(y,seqname=names(y),term=1){
  names(y)<-seqname
  background<-function(y){
    y1<-cumsum(y)
    (y1[2:length(y)]+y1[1:(length(y)-1)]) / 2
  }
  gmname<-"GM(1,1)"
  forecasts<-NA
  simulation<-NA
  response<-NA
  a<-NA
  b<-NA
  errors<-rep(0,length(y))
  names(errors)<-seqname
  f_errors<-NA
  list(gmname=gmname,original=y,background=background,a=a,b=b,
       response=response,simulation=simulation,term=term,forecasts=forecasts,
       errors=errors,f_errors=f_errors)
}
