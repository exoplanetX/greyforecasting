#' optimal gm model with background and response formula
#'
#' weighted background gm model and solved by auxillary parameters
#' @export
#' @param y data sequence.
#' @param present character vector containing xlab and ylab.
#' @param buff buffer operator used for original data.
#' @param alpha coefficient in buffer operator if used.
#' @examples
#' g<-gm_1(y,term=3)

gm_2 <- function(y,ntest=NULL,term=1,buff=NULL,alpha=NA){
#--原始数据截取ntest部分，生成建模序列x
  if(is.null(names(y))) names(y)<-1:length(y)
  if(is.numeric(ntest)){
    x<-y[1:(length(y)-trunc(ntest))]
    test<-y[(length(x)+1):length(y)]
  }else{
    x <- y
    test <- NULL
  }
  ny=length(y) #原始序列长度
  n=length(x) #x:建模序列长度
  nf=n+term #拟合+预测序列长度
  if(nf<ny){
    stop("ntest is too small or term is too big")
  }
##--缓冲处理，生成建模序列x0
  if(is.function(buff)) {
    if(is.na(alpha)) x0 <- buff(x) else x0 <- buff(x, alpha = alpha)
  }else{
    x0<-x
  }
##--建模处理，生成参数向量p['a']，p['b']
  x1=cumsum(x0)
  auxillary=LSE(x0[2:n],-x1[2:n],ones(n-1))
  a <- log(auxillary['a']+1)
  ax <- 1/a -1/auxillary['a']
  b <- auxillary['b']*a/auxillary['a']
  ##
  fupper<- sum(exp(-a*(1:n))*x0[1:n])
  flower<- sum(exp(-2*a*(1:n)))
  belta<-fupper/flower
  ##
  trf=function(k) belta*exp(-a*k)
  fitted_x0<-trf(1:n)
  fitted_x0[1]<-x0[1]
  names(fitted_x0)<-names(x0)
  forecasts<-trf(n+1:term)
  names(forecasts)<-as.numeric(names(x0)[n])+1:term

  obj<-list(
    data      =x,
    test      =test,
    parameter =data.frame(a=a,b=b,ax=ax),
    fitted    =fitted_x0[1:n],
    term      =term,
    forecasts =forecasts,
    mape.in   = mape(x0,fitted_x0[1:n]),
    mape.out  = ifelse(is.null(ntest), NA, mape(test,forecasts[1:ntest])),
    method     = list(class="gm",mdname=" OBT-GM(1,1)",buff=buff,alpha=alpha)
  )
  class(obj)<-"greyforecasting"
  obj
}

