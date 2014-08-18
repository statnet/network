#The following battery of tests is intended to verify the functionality of
#the network library

library(network)

# ----- check assigning multiple attribute values in a single call 
test<-network.initialize(3)
set.vertex.attribute(test,c('a','b'),c(1,2))
if(!all(test%v%'a'==c(1,1,1) & test%v%'b'==c(2,2,2))){
  stop('setting multiple attribute values with set.vertex.attribute failed')
}

test<-network.initialize(3)
set.vertex.attribute(test,list('a','b'),c(1,2))
if(!all(test%v%'a'==c(1,1,1) & test%v%'b'==c(2,2,2))){
  stop('setting multiple attribute values with set.vertex.attribute failed')
}

test<-network.initialize(3)
set.vertex.attribute(test,c('a','b'),list(c(1,2,3),c(4,5,6)))
if(!all(test%v%'a'==c(1,2,3) & test%v%'b'==c(4,5,6))){
  stop('setting multiple attribute values with set.vertex.attribute failed')
}

test<-network.initialize(3)
set.vertex.attribute(test,c('a','b'),list(list(1,2,3),list(4,5,6)))
if(!all(test%v%'a'==c(1,2,3) & test%v%'b'==c(4,5,6))){
  stop('setting multiple attribute values with set.vertex.attribute failed')
}

test<-network.initialize(3)
obj<-list(one='a complex object',two=c('with muliple','parts'))
set.vertex.attribute(test,c('a','b'),list(list(as.list(obj)),list(as.list(obj))))
if(!all(all.equal(get.vertex.attribute(test,'a',unlist=FALSE)[[1]],obj) & all.equal(get.vertex.attribute(test,'b',unlist=FALSE)[[1]],obj))){
  stop('setting multiple attribute values with list values in set.vertex.attribute failed')
}

# check assignment to list of networks
net <- network.initialize(2)
netlist <- list(net)
set.network.attribute(netlist[[1]],"test","a value")
if (!"test" %in% list.network.attributes(netlist[[1]]))
  stop('assignment to list of networks failed')

# check memory saftey with a big assignment
net<-network.initialize(100000)
set.vertex.attribute(net,LETTERS,LETTERS)

# test multiple assignment for network

test<-network.initialize(3)
set.network.attribute(test,c("a","b"),1:2)
if (!all(test%n%'a'==1,test%n%'b'==2)){
  stop('mulltiple attribute assignment failed for set.network.attribute')
}

test<-network.initialize(3)
set.network.attribute(test,list("a","b"),as.list(1:2))
if (!all(test%n%'a'==1,test%n%'b'==2)){
  stop('mulltiple attribute assignment failed for set.network.attribute')
}



# test multiple assignment for edges

test<-network.initialize(3)
add.edges(test,tail=1:3,head=c(2,3,1))
net<-test
set.edge.attribute(net,c("a","b"),1:2)
if (!all(net%n%'a'==1,net%n%'b'==2)){
  stop('mulltiple attribute assignment failed for set.edge.attribute')
}

net<-test
set.edge.attribute(net,c('a','b'),list(c(1,2,3),c(4,5,6)))
if(!all(net%e%'a'==c(1,2,3) & net%e%'b'==c(4,5,6))){
  stop('setting multiple attribute values with set.edge.attribute failed')
}

net<-test
set.edge.attribute(net,c('a','b'),list(list(1,2,3),list(4,5,6)))
if(!all(net%e%'a'==c(1,2,3) & net%e%'b'==c(4,5,6))){
  stop('setting multiple attribute values with set.edge.attribute failed')
}

net<-test
obj<-list(one='a complex object',two=c('with muliple','parts'))
set.edge.attribute(net,c('a','b'),list(list(as.list(obj)),list(as.list(obj))))
if(!all(all.equal(get.edge.attribute(net,'a',unlist=FALSE)[[1]],obj) & all.equal(get.edge.attribute(net,'b',unlist=FALSE)[[1]],obj))){
  stop('setting multiple attribute values with list values in set.edge.attribute failed')
}

# check memory saftey with a big assignment
net<-network.initialize(100000)
net<-add.edges(net,1:99999,2:100000)
set.edge.attribute(net,LETTERS,LETTERS)


# check get edge attribute overloading
net<-network.initialize(3)
add.edges(net,c(1,2,3),c(2,3,1))
set.edge.attribute(net,'test',"a")
if(!all(get.edge.attribute(net,'test')==c("a","a","a"))){stop("overloading of get.edge.attribute to get.edge.value not working correctly ")}

# check list output of get.edge.attribute with deleted.edges.omit
delete.edges(net,2)
set.edge.attribute(net,'foo','bar',1)
if(!all.equal(list('bar',NULL,NULL),get.edge.attribute(net,'foo',unlist=FALSE))){
  stop("deleted.edges.omit argument causing bad return values in get.edge.attribute ")
}
if(!all.equal(list('bar',NULL),get.edge.attribute(net,'foo',unlist=FALSE,deleted.edges.omit=TRUE))){
  stop("deleted.edges.omit argument causing bad return values in get.edge.attribute ")
}

# check unlisted output of get.edge.attribute with na.omit and deleted.edges.omit
if(!all.equal(c('bar',NA,NA),get.edge.attribute(net,'foo',unlist=TRUE,na.omit=FALSE,attr.na.omit=FALSE))){
  stop("na.omit argument causing bad return values in get.edge.attribute")
}
if(!all.equal(c('bar',NA),get.edge.attribute(net,'foo',unlist=TRUE,na.omit=FALSE,attr.na.omit=FALSE,deleted.edges.omit=TRUE))){
  stop("na.omit argument causing bad return values in get.edge.attribute")
}


# check for undirected loops getID cases #327 #609
net<-network.initialize(2,loops=TRUE,directed=FALSE)
net[1,1]<-1
net[1,2]<-1
net[2,2]<-1
if(get.edgeIDs(net,v=1,alter=1)!=1){
  stop("problem with get.edgeIDs on undirected network with loops")
}
if(get.edgeIDs(net,v=2,alter=2)!=3){
  stop("problem with get.edgeIDs on undirected network with loops")
}

net<-network.initialize(2,loops=TRUE,directed=FALSE)
net[1,2]<-1
if(length(get.edgeIDs(net,v=2,alter=2))>0){
  stop("problem with get.edgeIDs on undirected network with loops")
}

# --- tests for get induces subgraph additions
data(emon)
# extract the network of responders in MtStHelens network with interaction Frequency of 4
subG4<-get.inducedSubgraph(emon$MtStHelens,eid=which(emon$MtStHelens%e%'Frequency'==4))
if(network.size(subG4)!=24){
  stop('wrong size eid induced subgraph')
}

if (any(subG4%e%'Frequency'!=4)){
  stop('bad edges in eid induced subgraph')
}

# checks for error conditions
# can't specify eid with v or alter
# get.inducedSubgraph(v=1:2,emon$MtStHelens,eid=which(emon$MtStHelens%e%'Frequency'==4))
# get.inducedSubgraph(alter=1:2,emon$MtStHelens,eid=which(emon$MtStHelens%e%'Frequency'==4))
# get.inducedSubgraph(emon$MtStHelens,eid=200:300)


# ---- tests for specific bugs/edgecases -----

# ticket #180 (used to throw error if no edges exist)
set.edge.attribute(network.initialize(3),"test","a")

# check for network of zero size --used to give error ticket #255
set.vertex.attribute(network.initialize(0),'foo','bar')


# check for is.na.network problems #619
x2<-network.initialize(3)
x2[1,2]<-NA
if(is.na.network(x2)[1,2]!=1){
  stop('problem iwth is.na.netowrk')
}

# check for na problems in which.matrix.type #926
mat <- matrix(rbinom(200, 1, 0.2), nrow = 20)
naIndices <- sample(1:200, 20)
mat[naIndices] <- NA
nw <- network(mat)
