#' GM(1,1)模型完整处理过程
#' 默认返回1期预测值，即term=1
#' 修改pattern="model"，则返回包含所有参数的模型数据集
gmprocess<-function(y,term=1,pattern="forecast"){
  gdata<-gmodel(y,term)
  gdata<-gm(gdata)
  if(pattern=="forecast"){
    return(gdata$forecasts)
  }
  if(patter=="model"){
    return(gdata)
  }
}
