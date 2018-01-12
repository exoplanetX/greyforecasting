#' adaptive buffered grey  rolling model
f<- function(r){
  md<-roll(y,buff=operator,intensity =r)
  md$mape
}
r<-optimize(f,c(0.01,1))
g<-roll(y,buff=operator,intensity=r$minimum)
t<-rep(0,99)
for(i in 1:99){
  t[i]<-f(i*0.01)
}
plot(0.01*1:99,t,type="l",xlab = "buffer operator coefficient:r",ylab = "fitting accuracy")
arrows(0.82,0.04,r$minimum,r$objective,angle = 20)
text(0.82,0.042,"optimal point")
dev.new()
plot(g,forecast = T)
