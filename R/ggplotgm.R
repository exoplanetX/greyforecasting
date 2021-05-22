# program for abrgm model submitting to theory of system engineering and applications
#' @export
ggplotgm<- function(){
  require(reshape2)
  require(ggplot2)
  smldata<-data.frame(year=as.numeric(names(yy[5:16])),original=yy[5:16],
                      model1vsmodel5=c(g1$simulation[5:15],g1$forecasts),
                      model2vsmodel5=c(g2$simulation[5:15],g2$forecasts),
                      model3vsmodel5=c(g3$simulation,g3$forecasts),
                      model4vsmodel5=c(g4$simulation,g4$forecasts),
                      model5=c(g5$simulation,g5$forecasts)
  )
  longdata<-melt(data=smldata,value.name=c('mld'),id.vars = c('year'),variable.name = 'model')

  #p<-ggplot(data=longdata,aes(x=year,y=mld,colour=model))+geom_point()+geom_line()
  stp1<-melt(data=smldata,value.name='bechmarks',id.vars = c('year','original','model5'),variable.name = 'benchmark')
  stp2<-melt(data=stp1,value.name='values',id.vars=c('year','benchmark'),variable.name='datatype')

  p<-ggplot(data=stp2,aes(x=year,y=values,shape=datatype))+
    geom_point()+
    geom_line()+
    facet_wrap(~benchmark,ncol=2)
  p
}

