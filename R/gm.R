gm<-function(x,present="y",term=1,bg=background,buff=NULL,alpha=NA,...){
  if(is.function(buff)) {
    y<-buff(x,alpha=alpha)
  }else{
    y<-x
  }
  p<- lm(y[2:length(y)]~I(-bg(y)))$coefficients
  names(p)<-c("b","a")
  trf=function(k) ((y[1]-p['b']/p['a'])*(1-exp(p['a']))*exp(-p['a']*(k-1)))
  ftd<-trf(1:length(y))
  ftd[1]<-y[1]
  names(ftd)<-names(x)
  extroplation<-trf(length(y)+1:term)
  names(extroplation)<-as.numeric(names(y)[length(y)])+1:term
  obj<-list(
    original=x,
    description=present,
    background=bg,
    parameter=p,
    response=trf,
    simulation=ftd,
    term=term,
    forecasts=extroplation,
    mape=mape(y,ftd)
    )

  class(obj)<-"greyforecasting"
  obj
}

