gmobj<-function(y,term=1,seqname=names(y),bg=background,...){
  p<- lm(y[2:length(y)]~I(-bg(y)))$coefficients
  names(p)<-c("b","a")
  trf=function(k) ((y[1]-p['b']/p['a'])*(1-exp(p['a']))*exp(-p['a']*(k-1)))
  ftd<-trf(1:length(y))
  ftd[1]<-y[1]
  names(ftd)<-seqname
  extroplation<-trf(length(y)+1:term)

  obj<-list(
    original=y,
    background=bg,
    parameter=p,
    response=trf,
    simulation=ftd,
    term1=term,
    forecasts=extroplation
    )

  class(obj)<-"greyforecasting"
  obj
}

