#' Improved grey forecasting model with optimal background values
#'
#' weighted background gm model and solved by auxillary parameters
#'
#' @param x data sequence.
#' @param present character vector containing xlab and ylab.
#' @param buff buffer operator used for original data.
#' @param alpha coefficient in buffer operator if used.
#' @examples
#' g<-gm_1(y,term=3)
#' g2<-gm_1(y,present=c("xlab","ylab"))
#' @references
#' XU Ning, DANG Yao-guo, DING Song. Optimization method of background value in GM(1,1) model based on least error[J]. Control and Decision, 2015,30(12).

gm_1 <- function(y,ntest=NULL,term=1,present=c(NA,NA),buff=NULL,alpha=NA){
  if(is.numeric(ntest)) {
    x<-y[1:(length(y)-trunc(ntest))]
    testvalue<-y[(length(x)+1):length(y)]
  }else{
    x<-y
    testvalue<-NULL
  }
  if(length(present)==1){
    present <- c(NA,present)
  }else{
    present <- c(present[1],present[2])
  }

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
    testvalue=testvalue,
    description=data.frame(xlab=present[1],ylab=present[2]),
    background="Optimal background with theoratical least errors",
    parameter=data.frame(a=a,b=b,alpha=ax,au_a=auxillary['a'],au_b=auxillary['b']),
    response=trf,
    simulation=ftd,
    term=term,
    forecasts=extroplation,
    mape.insample=mape(y,ftd)
  )

  class(obj)<-"greyforecasting"
  obj
}

