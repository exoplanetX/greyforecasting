#' @export

svr<-function(y,ntest=NULL,buff=NA,alpha=NA,term=1){
  require(e1071)
  ny=length(y)
  if(is.numeric(ntest)){
    x<-y[1:(ny-trunc(ntest))]
    test<-y[(length(x)+1):ny]
  }else{
    x<-y
    test<-NULL
  }
  n=length(x)
  ds=data.frame(x0=x,t=1:n)
  svr.model<-svm(x0~t,data=ds,type="eps-regression",kernel="radial")
  mape.in<-mean(abs(ds$x0-svr.model$fitted)/ds$x0)
  pre=predict(svr.model,data.frame( t=(n+1):(n+term) ) )
  nm=as.numeric(names(x))
  names(pre)=nm[n]+1:term
  mape.out <- ifelse(is.null(ntest),NA,mape(test,pre[1:length(test)]))
  # obj <- list(
  #   fitted=svr.model$fitted,
  #   forecasts=pre,
  #   mape.in=mape.in,
  #   mape.out=mape.out
  # )
  obj<-list(
    data       = x,
    test       = test,
    parameter  = data.frame(a=svr.model$rho,b=svr.model$gamma,ax=NA),
    fitted     = svr.model$fitted,
    term       = term,
    forecasts  = pre,
    mape.in    = mape.in,
    mape.out   = mape.out,
    method     = list(class="gm",buff=buff,alpha=alpha)
  )
  class(obj)<-"greyforecasting"
  return(obj)
}
