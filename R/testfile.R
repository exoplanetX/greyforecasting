
y=c(135.84,178.29,321.12,551.36,606.11)
x=c(2001,2002,2003,2004,2005)
gdata<-gmodel(y,x,3)
gdata<-gm(gdata)
print(mape(gdata), digits=4)
p1<-gm.graph(gdata)
