#' classic grey forecasting model, GM(1,1)
#'
#' model sequential data with GM(1,1) model
#'
#' @examples
#' g<-gm(y)
#' @param x
#' x: data sequence.
#' @param present
#' present: character vector containing xlab and ylab.
#' @param bg
#' bg: background formula.
#' @param buff
#' buff: buffer operator used for original data.
#' @param alpha
#' alpha: coefficient in buffer operator if used.

gm<-function(y,ntest=NA,term=1,present=c(NA,NA),bg=background,buff=NULL,alpha=NA,...){
  if(is.numeric(ntest)) {
    x<-y[1:(length(y)-trunc(ntest))]
    testvalue<-y[(length(x)+1):length(y)]
  }else{
    x<-y
    testvalue<-NA
  }

  if(length(present)==1){
    present <- c(NA,present)
  }else{
    present <- c(present[1],present[2])
  }

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
    testvalue=testvalue,
    description=data.frame(xlab=present[1],ylab=present[2]),
    background=bg,
    parameter=p,
    response=trf,
    simulation=ftd,
    term=term,
    forecasts=extroplation,
    mape.insample=mape(y,ftd)
    )

  class(obj)<-"greyforecasting"
  obj
}

