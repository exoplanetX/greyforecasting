#'
#' @export
fgm<-function(y,ntest=NULL,term=1,r=1,bg=background,buff=NULL,alpha=NA,...){
  #--原始数据截取ntest部分，生成建模序列x
  if(is.null(names(y))){
    names(y) <- 1:length(y)
  }
  if(is.numeric(ntest)) {
    x<-y[1:(length(y)-trunc(ntest))]
    test<-y[(length(x)+1):length(y)]
  }else{
    x<-y
    test<-NULL
  }
  ny=length(y) #原始序列长度
  n=length(x) #建模序列长度
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
  x1=ago(x0,r)
  #p=LSE(x0[2:n],-bg(x1),ones(n-1))
  p=lm(x0[2:n]~I(-bg(x1)))$coefficients
  names(p)=c('b','a')
  ##--生成响应式序列ftd_x0和ftd_x1
  trf_x1=function(k) ( (x0[1]-p['b']/p['a'])*exp(-p['a']*(k-1))+p['b']/p['a'] )
  fitted_x1<-trf_x1(1:nf)
  fitted_x1[1]<-x0[1]
  fitted_x0=iago(fitted_x1,r)
  names(fitted_x0[1:n])<-names(x0)
  names(fitted_x0)=c(as.numeric(names(x0)),as.numeric(names(x0)[n])+1:term)
  forecasts<-fitted_x0[(n+1):nf]

  obj<-list(
    data       = x,
    test       = test,
    parameter  = data.frame(a=p['a'],b=p['b'],ax=0.5),
    fitted     = fitted_x0[1:n],
    term       = term,
    forecasts  = forecasts,
    mape.in    = mape(x0,fitted_x0[1:n]),
    mape.out   = ifelse(is.null(ntest), NA, mape(test,forecasts[1:ntest])),
    method     = list(name="Fractional GM(1,1)",class="gm",buff=buff,alpha=alpha)
  )

  class(obj)<-"greyforecasting"
  obj
}

