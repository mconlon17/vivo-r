# Copyright (c) 2015 Mike Conlon
# See LICENSE file for terms of use

make.vivo.uri<-function(r){ 
#
#   Given a uri string of the kind found on VIVO pages, return a full URI of an RDF page
#
#   Example:
#   Given  "http://vivo.ufl.edu/individual/n342"  return
#          "http://vivo.ufl.edu/individual/n342/n342.rdf"
#  
    s<-unlist(strsplit(r,"/"))
    # s[3] <- "test.vivo.ufl.edu"  # ugly workaround for testing
    s<-c(s,paste(s[length(s)],".rdf",sep=""))
    paste(s,sep="/",collapse="/")
}

get.vivo.name<-function(x){
#
#   Given an XML parse tree x (of a VIVO RDF page), return the first rdf:label which is
#   usually the "name" of the things represented by the page.
#
    xmlValue(getNodeSet(x,"//rdfs:label")[[1]])
}

get.vivo.type<-function(x,vocab="vivoweb"){
#
#   Given an XML parse tree x (of a VIVO RDF page), return the first rdf:type which 
#   is an rdf:resource defined in the specified vocabulary.  So, for example, for the UF College of 
#   Nursing, should return "College" if that is how the College of Nursing is represented
#   in the specific vocabulary.
#
    types<-getNodeSet(x,"//rdf:type")
    s<-sapply(types,function(el) xmlGetAttr(el,"rdf:resource"))
    i<-grep(vocab,s)
    if (length(i) >0 && i[1]>0) {
        if(vocab=="bibo") {  #ugly.  Bibo urls do not use the "standard" # separator
            unlist(strsplit(s[i[1]],"/"))[6]
            }
        else {
            unlist(strsplit(s[i[1]],"#"))[2]
        }
    }
    else NULL
}

get.vivo.network<-function(raw.uri){
#
#   Given a raw uri of an organization, return the network of organizations that report to
#   the specified organization.  
#
#   To Do:
#   Add options to stop at specified depth -- only down two levels, for example
#   add options to add people, grants, publications to the network
#
#
#   Create a root node
#
    depth <-0
    org<-get.vivo.org(raw.uri)
    net <-network.initialize(1,directed=F)
    net %v% "vertex.names" <- org$name
    net %v% "type"         <- org$type
    net %v% "depth"        <- depth
    
#
#   Recurse through the sub-orgs to add additional nodes
#
    net
}

get.person.in.position <- function(raw.uri){
#
#  Given the raw.uri of a position, return the person object of the person in the position
#
    uri    <- make.vivo.uri(raw.uri)
    x      <- xmlParse(uri)
    pset   <- getNodeSet(x,"//d:positionForPerson",c(d="http://vivoweb.org/ontology/core#"))
    if(length(pset) == 0 ) person <- NULL
    else {
      p.uri  <- sapply(pset,function(el) xmlAttrs(el)["resource"])
      person <- get.vivo.person(p.uri)
      }
    person  
}

get.vivo.org<-function(raw.uri){
#
#   Given a raw uri, return attributes of the organization at that uri.  subOrganizations
#   are returned as a list of raw uri.
#
    uri  <- make.vivo.uri(raw.uri)
    x    <- xmlParse(uri)
    name <- get.vivo.name(x)
    type <- get.vivo.type(x)
    s    <- getNodeSet(x,"//d:hasSubOrganization",c(d="http://vivoweb.org/ontology/core#"))
    subs <- sapply(s,function(el) xmlAttrs(el)["resource"])
    p    <- getNodeSet(x,"//d:organizationForPosition",c(d="http://vivoweb.org/ontology/core#"))
    pos  <- sapply(p,function(el) xmlAttrs(el)["resource"])
#
#   For each position, get the person in the position
#
    person <- sapply(pos,function(el) get.person.in.position(el))
#
#   get the number of people, pubs and grants for the organization
#
    if (length(person)==0) {
        people <- 0
        pubs <- 0
        grants <- 0
        }
    else {
        people <- ncol(person)
        pubs   <- sum(unlist(person["pubs",]))
        grants <- sum(unlist(person["grants",]))
        }

    list(name=name,type=type,subs=subs,pos=pos,person=person,people=people,pubs=pubs,grants=grants)
}

get.vivo.orgs.new<-function(raw.uri,depth=0){
#
#   Given the uri of a page referring to a VIVO organization, return a nested list of
#   subOrganizations and their subOrganizations recursively until all subOrganizations
#   of the given organization are included.  For each org, return name, type, depth, subOrgs
#
#   get.vivo.orgs can then called recursively on each subOrg to build a nested list of orgs.
#
    g<-get.vivo.org(raw.uri)
    u<-NULL
    if(length(g$subs)==0) list(name=g$name,type=g$type,depth=depth,pos=g$pos,person=g$person,
       people=g$people,pubs=g$pubs,grants=g$grants,subs=NULL)
    else {
        depth<-depth+1
        for(i in 1:length(g$subs)){
            sub.uri<-g$subs[[i]]
            u<-c(u,get.vivo.orgs.new(sub.uri,depth))
        }
        depth<-depth-1
        list(name=g$name,type=g$type,depth=depth,pos=g$pos,person=g$person,
          people=g$people,pubs=g$pubs,grants=g$grants,subs=u)
    }
}


get.vivo.orgs<-function(uri,depth){
#
#   Given the uri of a page referring to a VIVO organization, return a nested list of
#   subOrganizations and their subOrganizations recursively until all subOrganizations
#   of the given organization are included.  For each org, return name, type, depth, subOrgs
#
#   get.vivo.orgs can then called recursively on each subOrg to build a nested list of orgs.
#
    x<-xmlParse(uri)
    u<-NULL
    name<-get.vivo.name(x)
    type<-get.vivo.type(x)
    subs<-getNodeSet(x,"//d:hasSubOrganization",c(d="http://vivoweb.org/ontology/core#"))
    if(length(subs)==0) list(name=name,type=type,depth=depth,subs=NULL)
    else {
        depth<-depth+1
        for(i in 1:length(subs)){
            sub.uri<-make.vivo.uri(xmlAttrs(subs[[i]])["resource"])
            u<-c(u,get.vivo.orgs(sub.uri,depth))
        }
        depth<-depth-1
        list(name=name,type=type,depth=depth,subs=u)
    }
}

get.vivo.edges <- function(n,p){
# Recursive function for creating an edgelist from an org structure with 3 elements
  e<-NULL
  if (!is.null(p$subs)){
    for(i in 1:length(p$subs)){
      if (i %% 4 == 1) {
         a<-p$subs[i+1]$type
         type<-ifelse(is.null(a),"unknown",a)
         e<-c(e,n,type,p$subs[i+2]$depth,p$subs[i]$name,get.vivo.edges(p$subs[i]$name,p$subs[i+3]))
      }
    }
  }
  e
}

vivo.network<-function(n,p){
#
#   Given an org structure from get.vivo.orgs, create a network object with
#   vertext attributes collected from the crawl.
#
#   To do:  Generalize to multi-attribute list
#           Remove ugly patch for root node
#
    e<-get.vivo.edges(n,p)
    from <-e[seq(1,length(e),by=4)]
    type <-e[seq(2,length(e),by=4)]
    depth<-e[seq(3,length(e),by=4)]
    to   <-e[seq(4,length(e),by=4)]
    m<-matrix(c(from,to),length(from),2,byrow=F)
    vn<-network(m,directed=F)
    nm<-get.vertex.attribute(vn,"vertex.names")
#
# type is for get.vivo.edges.  Get type for each vertex
#
    to<-c(n,to)                 # ouch, ugly. Add root to for a list of vertices
    type<-c("University",type)  # ouch, uglier
    type<-type[match(nm,to)]    # get the type for the node by matching on name

    set.vertex.attribute(vn,"type",type)
#
# depth is for get.vivo.edges.  Get depth for each vertex
#
    depth<-c(0,depth)           # ouch, uglier.  Depth of root node
    depth<-depth[match(nm,to)]  # get the depth for the node by matching on name

    set.vertex.attribute(vn,"depth",as.numeric(depth))
    vn %n% "max.depth" <- max(depth) # handy
    vn
}

vivo.data.frame<-function(vn,exclude.na=T,expand=T){
#  
#   Given a VIVO Network Object, return a VIVO data.frame suitable for export
#   to Excel, or other row and column software
# 
#   exclude.na indicates whether the na attribute from the network object should
#   be included in the data.frame. Default is to exclude the na attribute
#   
#   Exclude indicates whether org data should be expanded to show the full path
#   to the org.  Default is to expand
#


    d<-NULL
    names<-list.vertex.attributes(vn)
    for(i in 1:length(names))
        if (names[[i]] != "na" || !exclude.na)
            d[[names[i]]] <- get.vertex.attribute(vn,names[i])
    d<-as.data.frame(d)
    if(expand){
        max.depth <- as.numeric(vn %n% "max.depth")
        m <- matrix(NA,dim(d)[1],max.depth,dimnames=list(NULL,
            paste(rep("P",max.depth),seq(1,max.depth),sep="")))
        el <- as.matrix(vn,matrix.type="edgelist")
        for (i.org in 1:nrow(d)){
            c.org<-i.org
            if (d[i.org,"depth"] > 0){
                for (i.depth in d[i.org,"depth"]:1){
                    p.org <- el[el[,2]==c.org,1][1] # first occurence if multiple parents
                    m[i.org,i.depth]<-as.character(d[p.org,"vertex.names"])
                    c.org<-p.org
                }
            }
        }
        d<-cbind(d,m)
    }
}
   

get.vivo.attribute <- function(x,attr,namespace="http://vivoweb.org/ontology/core#"){
#
#   Given the xml parse tree of a VIVO RDF page in x, find the first attribute attr in
#   the given namespace.  Return NA if no such attribute
#
#
    xns<-getNodeSet(x,paste("//d:",attr,sep="",collapse=""),c(d=namespace))
    ifelse(length(xns)==0,NA,xmlValue(xns[[1]]))
}

get.vivo.datetimeinterval <- function(x) {
#
#   Given the XML parse tree of a VIVO RDF page x, return the start and end
#   dates of the first datetime interval in the parse tree, otherwise NA for both
#
    xns<-getNodeSet(x,"//d:dateTimeInterval",c(d="http://vivoweb.org/ontology/core#"))
    if (length(xns)==0) list(start=NA,end=NA)
    else {
        dti.uri <- make.vivo.uri(xmlGetAttr(xns[[1]],"rdf:resource"))
        xi<-xmlParse(dti.uri)
#
#   get the start date for the time interval
#
        start.xns <- getNodeSet(xi,"//d:start",c(d="http://vivoweb.org/ontology/core#"))
        start.uri <- make.vivo.uri(xmlGetAttr(start.xns[[1]],"rdf:resource"))
        print(start.uri)
        start.x   <- xmlParse(start.uri)
        start     <- get.vivo.attribute(start.x,"dateTime")
    
#
#   process end
#
        end.xns   <- getNodeSet(xi,"//d:end",c(d="http://vivoweb.org/ontology/core#"))
        end.uri   <- make.vivo.uri(xmlGetAttr(end.xns[[1]],"rdf:resource"))
        end.x     <- xmlParse(end.uri)
        end       <- get.vivo.attribute(end.x,"dateTime")
#
#   return the start and end
#  
        fmt <- "%Y-%m-%dT%H:%M:%S"
        list(start=strptime(start,fmt),end=strptime(end,fmt))  
    }
}

get.vivo.grant.awarded.by<-function(x){
#
#   Given the XML parse tree of a VIVO RDF page in x, return the name of the first 
#   grantAwardBy agency, otherwise NA
#
    xns<-getNodeSet(x,"//d:grantAwardedBy",c(d="http://vivoweb.org/ontology/core#"))
    if (length(xns)==0) grant.award.by <- NA
    else {
        gab.uri <- make.vivo.uri(xmlGetAttr(xns[[1]],"rdf:resource"))
        x.gab<-xmlParse(gab.uri)
        grant.awarded.by <- get.vivo.attribute(x.gab,"label","http://www.w3.org/2000/01/rdf-schema#")
    }
}
 
get.vivo.grant<-function(raw.uri){
#
#   Given a raw.uri (without the rdf filetype) of a VIVO grant RDF page,
#   return basic facts about the grant.  Dereference as needed.  Missing
#   attributes are returned as NA
#
    uri <- make.vivo.uri(raw.uri)
    x <- xmlParse(uri)
    title              <- get.vivo.attribute(x,"label","http://www.w3.org/2000/01/rdf-schema#")
    sponsor.award.id   <- get.vivo.attribute(x,"sponsorAwardId","http://vivo.ufl.edu/ontology/vivo-ufl/")
    total.award.amount <- get.vivo.attribute(x,"totalAwardAmount")
    ps.contract.number <- get.vivo.attribute(x,"psContractNumber","http://vivo.ufl.edu/ontology/vivo-ufl/")
    dates              <- get.vivo.datetimeinterval(x)
    grant.awarded.by   <- get.vivo.grant.awarded.by(x)
    list(title=title,
        total.award.amount=total.award.amount,sponsor.award.id=sponsor.award.id,
        ps.contract.number=ps.contract.number,start=dates$start,end=dates$end,
        grant.awarded.by=grant.awarded.by)
}

get.vivo.publication<-function(raw.uri){
#
#   Given a raw.uri (without the rdf filetype) of a VIVO publication RDF page,
#   return basic facts about the publication.  Dereference as needed.  Missing
#   attributes are returned as NA
#
    uri <- make.vivo.uri(raw.uri)
    x <- xmlParse(uri)
    title     <- get.vivo.attribute(x,"label","http://www.w3.org/2000/01/rdf-schema#")
    authors   <- length(getNodeSet(x,"//d:informationResourceInAuthorship",c(d="http://vivoweb.org/ontology/core#")))
    bibo.type <- get.vivo.type(x,vocab="bibo")
#
#   deref the publication venue for its name
#
    xns<-getNodeSet(x,"//d:hasPublicationVenue",c(d="http://vivoweb.org/ontology/core#"))
    if (length(xns)==0) venue<-NA
    else {
        venue.uri <- make.vivo.uri(xmlGetAttr(xns[[1]],"rdf:resource"))
        xv<-xmlParse(venue.uri)
        venue          <- get.vivo.attribute(xv,"label","http://www.w3.org/2000/01/rdf-schema#")
    }

    list(title=title,authors=authors,bibo.type=bibo.type,venue=venue)
}

get.vivo.person<-function(raw.uri){
#
#   Given a raw.uri (without the rdf filetype) of a VIVO person page, return basic facts about the
#   the person.  Dereference as needed.  Missing attributes are returned as NA.
#
    uri <- make.vivo.uri(raw.uri)
    x <- xmlParse(uri)

    name.path <- paste("//rdf:Description[@rdf:about='",raw.uri,"']/rdfs:label",sep="",collapse="")
    xns<-getNodeSet(x,name.path)
    if (length(xns) == 0) name <- NA else name <- xmlValue(xns[[1]])

    era.commons.id <- get.vivo.attribute(x,"eraCommonsID","http://vivo.ufl.edu/ontology/vivo-ufl/")
    email <- get.vivo.attribute(x,"email")
    phone.number <- get.vivo.attribute(x,"phoneNumber")

    pubs <- length(getNodeSet(x,"//d:authorInAuthorship",c(d="http://vivoweb.org/ontology/core#")))
    grants <- length(getNodeSet(x,"//d:hasPrincipalInvestigatorRole",c(d="http://vivoweb.org/ontology/core#")))
#
#   deref first (!) personInPosition to get department name
#
    xns<-getNodeSet(x,"//d:personInPosition",c(d="http://vivoweb.org/ontology/core#"))
    if (length(xns) == 0) department <- NA 
    else {
      pip.uri <- make.vivo.uri(xmlGetAttr(xns[[1]],"rdf:resource"))
      xpip<-xmlParse(pip.uri)

      xns<-getNodeSet(xpip,"//d:positionInOrganization",c(d="http://vivoweb.org/ontology/core#"))
      if (length(xns) == 0) department <- NA
      else {
        pio.uri <- make.vivo.uri(xmlGetAttr(xns[[1]],"rdf:resource"))
        xpio <- xmlParse(pio.uri)
        department <- get.vivo.name(xpio)
        }
      }

    list(name=name,pubs=pubs,grants=grants,department=department,
        era.commons.id=era.commons.id,email=email,phone.number=phone.number,uri=raw.uri)
}
   


# Create the network of UF organizations

uf.uri<-"http://vivo.ufl.edu/individual/n1278130/n1278130.rdf"
uf.org<-get.vivo.orgs(uf.uri,0)      # crawl the RDF to build the org structure
uf.n<-vivo.network("UF",uf.org)      # create a network from the edge matrix
uf.t<-vivo.data.frame(uf.n)          # make a data.frame
write.csv(uf.t,file="uf-data.csv")   # write data to a CSV file for Excel/pivot
#
#  draw big (6 foot square), PDF of the UF org structure.  Color each node by distance
#  from UF node
#
pdf(file="BigUFDepth.pdf",height=72,width=72)
palette(rainbow(8))
plot(uf.n,displaylabels=T,vertex.col=get.vertex.attribute(uf.n,"depth"))
dev.off()
#
#  draw big (6 foot square), PDF of the UF org structure.  Color each node by type
#  
#
pdf(file="BigUFType.pdf",height=72,width=72)
palette(terrain.colors(25))
plot(uf.n,displaylabels=T,vertex.col=as.numeric(as.factor(get.vertex.attribute(uf.n,"type"))))
dev.off()

