# some really basic speed checks to help us know if we make changes that massively degrade performance
require(network)

testing<-FALSE  #Set to TRUE if you want to test
if(testing){
  init<-system.time(net<-network.initialize(100000))[3]
  setv<-system.time(set.vertex.attribute(net,"foo","bar"))[3]
  getv<-system.time(get.vertex.attribute(net,"foo"))[3]
  listv<-system.time(list.vertex.attributes(net))[3]
  adde<-system.time(add.edges(net,tail=1:99999,head=2:100000))[3]
  sete<-system.time(set.edge.attribute(net,"foo","bar"))[3]
  gete<-system.time(get.edge.attribute(net,"foo"))[3]
  liste<-system.time(list.edge.attributes(net))[3]
  addmoree<-system.time(add.edge(net,100000,1))[3]
  addmorev<-system.time(add.vertices(net,1))[3]


# optionally compare to benchmarks saved in test folder to see if things have changed
# benchmarks<-rbind(init,setv,getv,listv,adde,sete,gete,liste,addmoree,addmorev)
# oldmarks<-read.table(file.choose(),header=TRUE,colClasses=c('character','numeric'))
# all.equal(oldmarks[,1],benchmarks[,1],check.attributes=FALSE)

# optionally save out benchmarks to test directory
# write.table(benchmarks,file=file.choose())

# some absolute thresholds

  if(init>5){
    stop("initializing network for large number of vertices took much longer than expected")
  }

  if(setv>5){
    stop("set.vertex.attribute for large number of vertices took much longer than expected")
  }

  if(getv>5){
    stop("get.vertex.attribute for large number of vertices took much longer than expected")
  }

  if(listv>1){
    stop("list.vertex.attributes for large number of vertices took much longer than expected")
  }

  if(adde>5){
    stop("add.edges for a large number of edges took much longer than expected")
  }

  if(sete>10){
    stop("set.edge.attribute for a large number of edges took much longer than expected")
  }

  if(gete>1){
    stop("get.edge.attribute for a large number of edges took much longer than expected")
  }

  if(liste>1){
    stop("list.edge.attribute for a large number of edges took much longer than expected")
  }

  if(addmoree>5){
    stop("add.edge for a network with a large number of edges took much longer than expected")
  }

  if(addmorev>5){
    stop("add.vertices for a network with large number of vertices took longer than expected")
  }
}
