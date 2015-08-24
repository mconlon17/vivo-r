#
# Learning to use network and network plotting functions for VIVO data
#
# Copyright (c) 2015 Mike Conlon
# See LICENSE file for terms of use
#
#  To Do
#  Add grant dollars as an attribute.  Derive attribute for People, Department, College
#  Size of node is log of attribute
#
#  Experiment with "large" sizes in PDF (poster size plots with labeling)
#
# start with a root node
net <-network.initialize(1,directed=F)
net %v% "type" <- "Root"
net %v% "vertex.names" <- "COM"
net %v% "amt" <-0
#
# create some departments
#
add.vertices(net,1,list(list(type="Department",vertex.names="Radiology",amt=0)))
net["COM","Radiology"]<-1
add.vertices(net,1,list(list(type="Department",vertex.names="Pathology",amt=0)))
net["COM","Pathology"]<-1
add.vertices(net,1,list(list(type="Department",vertex.names="Pediatrics",amt=0)))
net["COM","Pediatrics"]<-1
#
# create some people in the departments
#
add.vertices(net,1,list(list(type="Person",vertex.names="Mancuso",amt=0)))
net["Radiology","Mancuso"]<-1
add.vertices(net,1,list(list(type="Person",vertex.names="Sistrom",amt=0)))
net["Radiology","Sistrom"]<-1 
add.vertices(net,1,list(list(type="Person",vertex.names="Rathe",amt=0)))
net["Radiology","Rathe"]<-1 

for (i in 1:15) {
  name <- paste("P",i,sep="")
  add.vertices(net,1,list(list(type="Person",vertex.names=name,amt=0)))
  net["Pathology",name]<-1
}
for (i in 1:30) {
  name <- paste("M",i,sep="")
  add.vertices(net,1,list(list(type="Person",vertex.names=name,amt=0)))
  net["Pediatrics",name]<-1
}
#
# Add grants as another level
#
vt<-get.vertex.attribute(net,"type")
vn<-get.vertex.attribute(net,"vertex.names")
va<-get.vertex.attribute(net,"amt")
for (i in 1:network.size(net)) {
  if (vt[i] == "Person") {
    di<-get.neighborhood(net,i)[1]  # get department index for this person
    ng <- floor(10*runif(1))   
    if (ng > 0) {
      for (j in 1:ng) {
        gname <- paste(vn[i],".G",j,sep="")
        gamt=floor(50000*rexp(1)*rexp(1))
        add.vertices(net,1,list(list(type="Grant",vertex.names=gname,amt=gamt)))
        net[vn[i],gname]<-1
#
# Add the grant amounts to the corresponding person grant total
#
        va<-get.vertex.attribute(net,"amt")
        va[i]<-va[i]+gamt
        set.vertex.attribute(net,"amt",va)
#
# Add the grant amounts to the corresponding department total
#
        va<-get.vertex.attribute(net,"amt")
        va[di]<-va[di]+gamt
        set.vertex.attribute(net,"amt",va)
#
# Add the grant amount to the root total
#
        va<-get.vertex.attribute(net,"amt")
        va[1]<-va[1]+gamt
        set.vertex.attribute(net,"amt",va)
      }
    }
  }
}

#
#   Default plot
#
plot(net)
#
#   Color nodes by type
#
plot(net,vertex.col="type")
#
#   Color nodes by type and size them by grant dollars
#
palette(rev(rainbow(6)))
plot(net,vertex.col="type",
  vertex.cex=0.5*(1+floor(log10(1+get.vertex.attribute(net,"amt")))))