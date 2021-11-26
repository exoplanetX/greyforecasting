#' adaptive buffered grey rolling model
#' @export
#' @examples
#' r<-roll(y)
#' @param y data sequence
#' @param ntest test data of outsample
#' @param model unit grey model used in each data piece
#' @param buff the buffer operator function adjusting data piece
#' @param rollterm forecasting terms by extroplation
#' @param piece length of data piece
#' @param stepsize  rolling step

roll <- function(y,ntest=NULL,rollterm=1,model=gm,buff=NA,intensity=NA,piece=4,stepsize=1){
#---数据准备-------
  ny=length(y)
  if(is.null(names(y)))    names(y)<-1:ny
  if(is.numeric(ntest)){
    x<-y[1:(ny-trunc(ntest))]
    test<-y[(length(x)+1):ny]
  }else{
    x<-y
    test<-NULL
  }
  n=length(x) #x:建模序列长度
  nf=n+rollterm #拟合+预测序列长度
#--错误检查--------
  if(nf<ny)    stop("ntest is too small or term is too big")
  if(piece<4)  stop("piece cannot be less than 4")
#---生成滚动数据，逐段组合为data.frame
 # n <- length(x)
  nm=as.numeric(names(y))
  rollnumber<-(n-piece+1)/stepsize  #计算滚动次数
  rollset<-data.frame(x[1:piece])
  for(i in 2:rollnumber){
      rollset<-cbind(rollset,x[(1+(i-1)*stepsize):(piece+(i-1)*stepsize)])
  }
#---对每个片段施加缓冲算子
  if(is.function(buff)){
    rollsetf<-as.data.frame(lapply(rollset,buff,alpha=intensity))
  }
  else{
    rollsetf=rollset
  }
#---对缓冲后数据建模预测
  simulation<-unlist(lapply(rollsetf,gmprocess,model=model))
  names(simulation) <- c(nm[(piece+1):n],nm[n]+1)
  param <- lapply(rollsetf,gmprocess,model=model,pattern="parameter")
  paras <- data.frame(a=param[[1]]['a'],b=param[[1]]['b'],ax=param[[1]]['ax'])
  for(i in 2:length(param)){
    paras<-rbind(paras,data.frame( a=param[[i]]['a']  ,b=param[[i]]['b'],ax=param[[i]]['ax']))
  }
  row.names(paras)=1:rollnumber
  target=x[(piece+1):length(x)]
  fitness=mape(target,simulation[1:length(target)])
#---外推滚动--------

  if (is.function(buff)) {
    if (rollterm > 1) {
      for (j in 1:(rollterm - 1)) {
        rollsetf[[rollnumber + j]] <-
          buff(c(rollsetf[[rollnumber + j - 1]][2:piece], simulation[length(simulation)]),
               alpha = intensity)
        simulation[rollnumber + j] <-
          gmprocess(rollsetf[[rollnumber + j]], model = model)
        names(simulation)[rollnumber + j] <- nm[n] + j + 1
        temp2=gmprocess(rollsetf[[rollnumber + j]], model = model,pattern = "parameter")
        paras <- rbind(paras,data.frame(a=temp2$a,b=temp2$b,ax=temp2$ax))
      }
    }
  } else{
    if(rollterm>1){
      for(j in 1:(rollterm-1)){
        rollsetf[[rollnumber+j]]<-c(rollsetf[[rollnumber+j-1]][2:piece],simulation[length(simulation)])
        simulation[rollnumber+j]<-gmprocess(rollsetf[[rollnumber+j]],model=model)
        names(simulation)[rollnumber+j]<-nm[n]+j+1
        temp2=gmprocess(rollsetf[[rollnumber + j]], model = model,pattern = "parameter")
        paras <- rbind(paras,data.frame(a=temp2$a,b=temp2$b,ax=temp2$ax))
      }
    }
  }

#---return"obj"----
  obj<-list(
    data        = x,
    test        = test,
    parameter   = paras,
    fitted      = simulation[1:(rollnumber-1)],
    forecasts   = simulation[rollnumber+0:(rollterm-1)],
    term        = rollterm,
    mape.in     = fitness,  #mape(x[(piece+1):n],simulation[1:(length(simulation)-1)]),
    mape.out    = ifelse(is.null(ntest), NA, mape(test,simulation[rollnumber+0:(rollterm-1)])),
    method      = list(name="grey rolling model",class='roll',piece=piece,stepsize=stepsize,buff=buff)
  )
  class(obj)<-c("greyforecasting")
  return(obj)
}
