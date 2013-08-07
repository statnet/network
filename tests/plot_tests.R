# various tests for network plotting functions
# mostly recent functionality added by skyebend
require(network)
# -----  test edge labels ------
ymat<-matrix(c(0,1,2,3, 0,0,0,0, 0,0,0,0, 0,0,0,0),ncol=4)
ynet<-network(ymat,ignore.eval=FALSE,names.eval='weight')
# don't do anything if no value given
plot(ynet,edge.label.col='blue',edge.label.cex='weight')
# use edge ids is if edge.label=TRUE
plot(ynet,edge.label=TRUE)

plot(ynet,edge.label='weight',edge.label.col='blue',edge.label.cex='weight')

data(emon)
par(mar=c(0,0,0,0))
plot(emon[[5]],edge.label=TRUE,edge.label.cex=0.6,edge.col='gray',edge.lwd=(emon[[5]]%e%'Frequency')*2)