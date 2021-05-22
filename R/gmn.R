#' GM(1,N)
#'
#' Grey prediction model with n input variables. uncompleted
#'
#' @export
#' @examples
#' g<-gmn(y)
#' @param x data sequence.
#' @param xn driven sequence.
#' @param bg background formula.
#' @param buff buffer operator used for original data.
#' @param alpha coefficient in buffer operator if used.

gmn<-function(y,xn,ntest=NULL,term=1,bg=background,buff=NULL,alpha=NA,...){
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

  nd=length(xn) #驱动序列的个数
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
  x1=cumsum(x0)
  xn1=list()
  for(i in 1:nd){
    xn1[[i]]=ago(xn[[i]])
  }
  p=LSEn(x0[2:n],-bg(x1),ones(n-1),xn1)
  #-响应式

  trf.x1=function(k){
    for(i in nd){
      s=p[1+i]*xn1[[i]][k]
    }
    result=(x0[1]-s/p[1])*exp(-p['a']*k)+s/p[1]
  }


}
