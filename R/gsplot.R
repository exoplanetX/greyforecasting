#' plot fitting graph of grey models based on ggplot2
#' plot multiple of single grey model
#'
#' @export
#' @param lastpoint the cut line of x axis for fitting presentation.
#' @param xintercept divide line of insample part and forecasting part.
#' @param legend a charactor vector for model's name.
#' @param point logical parameter, TRUE: present points;FALSE: hide the points.
#' @param pointsize size of the data point.
#' @param linesize siez of the lines.
#' @param legend customized position of the legend,vector containing two elements.
#' @param save logical parameter, chooing TRUE will save the figure as pdf file.
#' @param n_break breaks of x axis, default will use breaks_pretty() in package scales.

gsplot <- function(...,lastpoint=NA,xintercept=NA,point=TRUE,
                   pointsize=1.2,linesize=0.9,legend=NA,
                   save=FALSE,n_breaks=NA){
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
  shapevalue=20+1:(num+1)
  p=ggplot(solution,aes(year,value,group=model))+
    geom_line(aes(linetype=model,color=model),size=linesize)
  if(point==T){
    p=p+geom_point(aes(shape=model),size=pointsize,fill="white")+
      scale_shape_manual(values = shapevalue) #c(21,22,23,24))
  }
    p=p+theme_classic()+
      theme(legend.position = c(0.15,0.8))
  #添加横坐标标签
  if(!is.na(n_breaks)){
    p=p+scale_x_continuous(breaks = breaks_pretty(n_breaks))
  }else{
    p=p+scale_x_continuous(breaks = breaks_pretty())
  }
  #添加拟合和预测区域的分割线
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
