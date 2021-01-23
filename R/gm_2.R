#' optimal gm model with background and response formula
#'
#' weighted background gm model and solved by auxillary parameters
#' @export
#' @param x data sequence.
#' @param present character vector containing xlab and ylab.
#' @param buff buffer operator used for original data.
#' @param alpha coefficient in buffer operator if used.
#' @examples
#' g<-gm_1(y,term=3)
#' g2<-gm_1(y,present=c("xlab","ylab"))

gm_2 <- function(y,ntest=NULL,term=1,present=c(NA,NA),buff=NULL,alpha=NA){
  if(is.null(names(y))){
    names(y)<-1:length(y)
  }
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
  ##

  fupper<- sum(exp(-a*(1:n))*y[1:n])
  flower<- sum(exp(-2*a*(1:n)))
  belta<-fupper/flower

  ##
  trf=function(k) belta*exp(-a*k)
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

