#' adaptive buffered grey  rolling model
#'
#' generate ABGR model, computation results and its fitting graphes
#' @param y raw data for modelling
#' @param model unit grey model used in data piece
#' @param buff buffer operator used to data piece
#' @param term forecasting term by extroplation
#' @examples
#' mymodel(dataset=y,model=gm_1,buff=operator,term=3)
abgr<- function(y=y,ntest=NULL,model=gm,buff=operator,term=1){
  #y<-dataset
  f<- function(r){
    md<-roll(y=y,ntest=ntest,model=model,buff=buff,intensity=r,rollterm=term)
    md$mape
  }
  r<-optimize(f,c(0.01,1))
  g<-roll(y,ntest=ntest,model=model,buff=buff,intensity=r$minimum,rollterm = term)
  t<-rep(0,99)
  for(i in 1:99){
    t[i]<-f(i*0.01)
  }

  plot(0.01*1:99,t,type="l",xlab = "buffer operator coefficient:r",ylab = "fitting accuracy")
  arrows(0.82,0.04,r$minimum,r$objective,angle = 20)
  text(0.82,0.042,"optimal point")
  dev.new()
  plot(g,forecast = T)
  cat(which.min(t))
  return(g)

}

