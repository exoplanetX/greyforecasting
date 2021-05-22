#'
#'
save_roll <- function(md,path="~/",filename=NULL){
  require(xlsx)
  if(length(md$test)>0){
    original=md$data
  }else{
    original=c(md$data,md$test)
  }
  origin=md$data
  fit=c(md$fitted,md$forecasts)
  temp=c(md$data,md$forecasts)
  nm=names(temp)
  n=length(temp)
  piece=md$method$piece
  #--存储表
  dt=data.frame(
    id=rep(NA,n),
    label=rep('outsample',n),
    original=rep(NA,n),
    fitting=rep(NA,n),
    errors=rep(NA,n),
    ape=rep(NA,n),
    insample=rep(NA,n),
    outsample=rep(NA,n),
    a=rep(NA,n),
    b=rep(NA,n),
    ax=rep(NA,n)
    )
  #--存储赋值
  dt$id=as.numeric(nm)
  dt$label[1:length(origin)]=rep('insample',length(origin))
  dt$original[1:length(original)]=original
  dt$fitting=c(rep(NA,md$method$piece),fit)
  dt$errors[(md$method$piece+1):length(origin)]=origin[(md$method$piece+1):length(origin)]-md$fitted
  if(length(md$test)>0) dt$errors[(length(origin)+1):n]=md$test-fit[(length(origin)+1):n]
  dt$ape[(md$method$piece+1):length(origin)]=ape(origin[(md$method$piece+1):length(origin)],md$fitted)
  if(length(md$test)>0) dt$ape[(length(origin)+1):n]=ape(md$test,md$forecasts)
  dt$insample[1]=md$mape.in
  dt$outsample[1]=md$mape.out
  dt$a=c(rep(NA,piece),md$parameter$a)
  dt$b=c(rep(NA,piece),md$parameter$b)
  dt$ax=c(rep(NA,piece),md$parameter$ax)
  # dt$a[1:length(md$parameter$a)]=md$parameter$a #
  # dt$b[1:length(md$parameter$b)]=md$parameter$b #
  # dt$ax[1:length(md$parameter$ax)]=md$parameter$ax #
  #--输出文件
  if(!is.null(filename)){
    savename=paste("results","-",filename,".xlsx")
  }else{
    savename="results.xlsx"
  }
  filepath=paste(path,savename)
  write.xlsx(dt,file = filepath,row.names = F,sheetName="Sheet1",showNA = F)
}
