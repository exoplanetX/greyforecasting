#multi-projection GM(1,1)
#' @export
mpgm<-function(y,ntest=NULL,term=1,bg=background1,buff=NULL,alpha=NA){
  #--数据检验与整理
  require(algorithm)
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
  #--模型参数计算
  ## 计算级比
  dta = x0[2:n] / x0[1:(n - 1)]
  w = (max(dta) - min(dta)) / mean(dta)
  x1 = cumsum(x0)
  pr=LSE(x0[2:n],-bg(x1,w),ones(n-1))
  pl=LSE(x0[2:n],-bg(x1,-w),ones(n-1))
  trf_r = function(k)
    (1 - exp(pr['a'])) * (x0[1] - pr['b'] / pr['a']) * exp(-pr['a'] * (k - 1))
  trf_l = function(k)
    (1 - exp(pl['a'])) * (x0[1] - pl['b'] / pl['a']) * exp(-pl['a'] * (k - 1))
  #适应度函数f, u为参数组合向量
  f = function(u)
    sum(abs(u[1] * trf_r(2:n) + u[2] * trf_l(2:n) - x0[2:n]))
  #--生成响应数据
  u = DE(2, f, lower = c(0, 0), upper = c(1, 1))
  trf = function(k)  u[1] * trf_r(k) + u[2] * trf_l(k)
  ftd <- trf(1:n)
  ftd[1] <- x0[1]
  extroplation <- trf(length(x0) + 1:term)
  names(ftd) <- names(x0)
  names(extroplation) <- as.numeric(names(x0)[length(x0)]) + 1:term

  #--生成类
  obj<-list(
    data       = x,
    test       = test,
    parameter  = as.data.frame(rbind(pr,pl)),
    fitted     = ftd[1:n],
    term       = term,
    forecasts  = extroplation,
    mape.in    = mape(x0,ftd[1:n]),
    mape.out   = ifelse(is.null(ntest), NA, mape(test,forecasts[1:ntest])),
    method     = list(class="mpgm",buff=buff,alpha=alpha,w=w)
  )
  class(obj)<-"greyforecasting"
  obj
}
