#' Improved grey forecasting model with optimal background values
#'
#' weighted background gm model and solved by auxillary parameters
#' @export
#' @param x data sequence.
#' @param present character vector containing xlab and ylab.
#' @param buff buffer operator used for original data.
#' @param alpha coefficient in buffer operator if used.
#' @examples
#' g<-gm_1(y,term=3)
#' @references
#' XU Ning, DANG Yao-guo, DING Song. Optimization method of background value in GM(1,1) model based on least error[J]. Control and Decision, 2015,30(12).

gm_1 <- function(y,ntest=NULL,term=1,buff=NULL,alpha=NA){
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
  a <- log(auxillary['a']+1) #还原参数a
  ax <- 1/a -1/auxillary['a']  #背景值组合参数
  b <- auxillary['b']*a/auxillary['a'] #还原参数b
  trf=function(k) ((x0[1]-b/a)*(1-exp(a))*exp(-a*(k-1)))
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
    method     = list(class="gm",buff=buff,alpha=alpha)

  )
  class(obj)<-"greyforecasting"
  obj
}

