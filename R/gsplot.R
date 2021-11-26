#' plot fitting graph of grey models based on ggplot2
#' plot multiple of single grey model
#'
#' @export
#' @param lastpoint the cut line of x axis for fitting presentation.
#' @param xintercept divide line of insample part and forecasting part.
#' @param legend a charactor vector for model's name.

gsplot <- function(...,lastpoint=NA,xintercept=NA,legend=NA,save=F,n_breaks=NA){
  require(ggplot2)
  require(ggthemes)
  require(tibble)
  require(ggsci)
  require(scales)
  require(showtext)
  require(dplyr)
  showtext_auto()

  md=list(...)
  num=length(md)
  #xintercept=2018

  originaldata=c(md[[1]]$data,md[[1]]$test)
  solution=data.frame(
    year=as.numeric(names(originaldata)),
    value=originaldata,
    model=rep("Original",length(originaldata))
  )

  lastname="Original"
  for(i in 1:num){
    fitvalue=c(md[[i]]$fitted,md[[i]]$forecasts)
    year=as.numeric(names(fitvalue))
    if(is.character(legend)){
      md[[i]]$method$name=legend[i]
    }
    if(md[[i]]$method$name==lastname){
      md[[i]]$method$name=paste(lastname,as.character(i))
    }
    dt=data.frame(
      year=year,
      value=fitvalue,
      model=rep(md[[i]]$method$name,length(fitvalue))
    )
    lastname=md[[i]]$method$name
    solution=rbind(solution,dt)
  }
  if(is.numeric(lastpoint)){
    if(lastpoint<min(solution$year)){
      stop("lastpoint is less than the start year!")
    }
    solution=solution %>% filter(year<=lastpoint)
  }
  #绘图

  p=ggplot(solution,aes(year,value,group=model))+
    geom_line(aes(linetype=model,color=model),size=0.9)+
    #geom_vline(aes(xintercept=xintercept),linetype=2)+
    geom_point(aes(shape=model),size=1.2,fill="white")+
    scale_shape_manual(values = c(21,22,23,24))+
    theme_classic()+
    theme(legend.position = c(0.15,0.8))

  if(!is.na(n_breaks)){
    p=p+scale_x_continuous(breaks = breaks_pretty(n_breaks))
  }else{
    p=p+scale_x_continuous(breaks = breaks_pretty())
  }
  if(is.numeric(xintercept)){
    ylevel=min(md[[1]]$data)+0.1*(max(md[[1]]$data)-min(md[[1]]$data))
    p=p+geom_vline(aes(xintercept=xintercept),linetype=2)+
      annotate("text",x=xintercept-3,y=ylevel,label="拟合数据")+
      annotate("text",x=xintercept+3,y=ylevel,label="预测数据")
  }
  if(save==T){
    ggsave(filename = "ceadsforecast.pdf",dpi=600)
  }
  return(p)
}
