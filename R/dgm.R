#' discrete grey prediction model by Xie Naiming
#'
#' model sequential data with discrete GM(1,1) model
#' @export
#' @examples
#' g<-dgm(y)

#' @param y original data
#' @param ntest number of insample data to test accuracy
#' @param buff buffer operator used for original data.
#' @param term length of extropotation data, forecasting data.


dgm<-function(y,ntest=NULL,term=1,buff=NULL,alpha=NA){
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
  p<- lm(x1[2:length(x1)]~x1[1:(n-1)])$coefficients
  names(p)<-c("b","a")
##--生成响应式序列fitted_x0和fitted_x1
  fitted_x0=c()
  fitted_x1=c()
  fitted_x0[1] <- x0[1]
  fitted_x1[1] <- x0[1]
  for(i in 2:nf){
    if(i<=n){
      fitted_x1[i] <- p['a']*x1[i-1]+p['b']
    } else {
      fitted_x1[i] <-p['a']*fitted_x1[i-1]+p['b']
    }
    fitted_x0[i] <- fitted_x1[i]-fitted_x1[i-1]
  }
  forecasts<-fitted_x0[(n+1):nf]
  names(fitted_x0)<-names(x) #??
  names(forecasts)<-as.numeric(names(x)[n])+1:term



#--生成类
  obj<-list(
    data      =x,
    test      =test,
    parameter =data.frame(a=p['a'],b=p['b'],ax=NA),
    fitted    =fitted_x0[1:n],
    term      =term,
    forecasts =forecasts,
    mape.in   = mape(x0,fitted_x0[1:n]),
    mape.out  = ifelse(is.null(ntest), NA, mape(test,forecasts[1:ntest])),
    method     = list(class="dgm",mdname="DGM(1,1)",buff=buff,alpha=alpha)
  )
  class(obj)<-"greyforecasting"
  obj
}
