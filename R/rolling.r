rolling<-function(y,x=1:length(y),rollterm=3,piece=4,stepsize=1){
  # 按piece和stepsize取出数据片，生成数据集
  n<-length(y)
  names(y)<-x
  rollnumber<-(n-piece+1)/stepsize
  for(i in 1:rollnumber){
    if(i==1){
      rollset<-data.frame(y[1:piece])
    }else{
      rollset<-cbind(rollset,y[(1+(i-1)*stepsize):(piece+(i-1)*stepsize)])
    }
  }
  names(rollset)<-letters[1:length(rollset)]
  #求出数据切片的预测值，即原数据的拟合值
  simulations<-unlist(lapply(rollset,gmprocess))
  names(simulations)<-c(x[(piece+1):n],x[n]+1)
  #进行滚动外推，预测原数据集
  if(rollterm>1){
    for(j in 1:(rollterm-1)){
      rollset[[rollnumber+j]]<-c(rollset[[rollnumber+j-1]][2:piece],simulations[rollnumber+j-1])
      simulations[rollnumber+j]<-gmprocess(rollset[[rollnumber+j]])
      names(simulations)[rollnumber+j]<-x[n]+j+1
    }
  }
  simulations

}
