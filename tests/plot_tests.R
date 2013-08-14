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

col.list<-c('red','#800000','#80000505',NA)
# test is.color for vector NA processing bug #491
if(!all(is.color(col.list)[1:3] & is.na(is.color(col.list)[4]))){
  stop('is.color did not correctly recognize colors and NA values in a character vector')
}
   
col.list<-list('red','#800000','#80000505',NA)
# test is.color for list NA processing bug #491
if(!all(is.color(col.list)[1:3] & is.na(is.color(col.list)[4]))){
  stop('is.color did not correctly recognize colors and NA values in a list')
}

