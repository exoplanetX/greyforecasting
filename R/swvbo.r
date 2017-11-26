#' 平滑变权缓冲算子
#' y:原始数据 alpha:变权权重
#' 调用格式：y<-svwbo(y,alpha=0.5)
#' 默认alpha=0.5
svwbo<-function(y,alpha=0.5){
  for(i in rev( seq_along(y[1:(length(y)-1)]) ) ){
    y[i]<-alpha*y[i]+(1-alpha)*y[i+1]
  }
  y
}
