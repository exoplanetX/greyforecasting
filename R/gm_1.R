#' weighted background gm model and solved by auxillary parameters
#' XU Ning, DANG Yao-guo, DING Song. Optimization method of background value in GM(1,1) model based on least error[J]. Control and Decision, 2015,30(12).
#'
gm_1 <- function(x,present="y",term=1,buff=NULL,alpha=NA){
  n<-length(x)
  if(is.function(buff)){
    y<-buff(x,alpha=alpha)
  }else{
    y<-x
  }



  auxillary<- lm(y[2:n]~I(-cumsum(y)[2:n]))$coefficients
  names(auxillary)<-c("b","a")
  a <- log(auxillary['a']+1)
  ax <- 1/a -1/auxillary['a']
  b <- auxillary['b']*a/auxillary['a']
  trf=function(k) ((y[1]-b/a)*(1-exp(a))*exp(-a*(k-1)))
  ftd<-trf(1:length(y))
  ftd[1]<-y[1]
  names(ftd)<-names(y)
  extroplation<-trf(length(y)+1:term)
  names(extroplation)<-as.numeric(names(y)[length(y)])+1:term
  obj<-list(
    original=x,
    description=present,
    background="Optimal background with theoratical least errors",
    parameter=data.frame(a=a,b=b,alpha=ax,au_a=auxillary['a'],au_b=auxillary['b']),
    response=trf,
    simulation=ftd,
    term=term,
    forecasts=extroplation,
    mape=mape(y,ftd)
  )

  class(obj)<-"greyforecasting"
  obj
}

