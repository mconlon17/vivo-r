# Copyright (c) 2015 Mike Conlon
# See LICENSE file for terms of use

Sys.time()
#  Generate Organizational structure for the University of Florida
#  Color nodes by an attribute of the organization

# Load SPARQL library
library(SPARQL)

# Load Statnet library
library(statnet)

# Clear workspace
rm(list=ls())

fix.vivo.label <- function(label) {
    # given a label returned from VIVO, strip off any language modifier
	regex <- "^\"(.+)\"@en-US$"
    label <- sub(regex,"\\1",label)
    }

fix.vivo.type <- function(type) {
    # given a type returned from VIVO, strip off any prefix
	regex<-".+/(.+)>$"
    type <- sub(regex,"\\1",type)
	regex <- ".+#(.+)$"
	type <- sub(regex,"\\1",type)
    }	

# Set SPARQL endpoint
endpoint <- "http://sparql.vivo.ufl.edu/VIVO/sparql"

# Pre-assign query prefixes (common to all queries)
prefix <- "
PREFIX rdf:      <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:     <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd:      <http://www.w3.org/2001/XMLSchema#>
PREFIX owl:      <http://www.w3.org/2002/07/owl#>
PREFIX swrl:     <http://www.w3.org/2003/11/swrl#>
PREFIX swrlb:    <http://www.w3.org/2003/11/swrlb#>
PREFIX vitro:    <http://vitro.mannlib.cornell.edu/ns/vitro/0.7#>
PREFIX bibo:     <http://purl.org/ontology/bibo/>
PREFIX c4o:      <http://purl.org/spar/c4o/>
PREFIX cito:     <http://purl.org/spar/cito/>
PREFIX event:    <http://purl.org/NET/c4dm/event.owl#>
PREFIX fabio:    <http://purl.org/spar/fabio/>
PREFIX foaf:     <http://xmlns.com/foaf/0.1/>
PREFIX geo:      <http://aims.fao.org/aos/geopolitical.owl#>
PREFIX p1:       <http://purl.org/dc/elements/1.1/>
PREFIX p2:       <http://purl.org/dc/terms/>
PREFIX obo:      <http://purl.obolibrary.org/obo/>
PREFIX ocrer:    <http://purl.org/net/OCRe/research.owl#>
PREFIX ocresd:   <http://purl.org/net/OCRe/study_design.owl#>
PREFIX p3:       <http://vivoweb.org/ontology/provenance-support#>
PREFIX skos:     <http://www.w3.org/2004/02/skos/core#>
PREFIX ufVivo:   <http://vivo.ufl.edu/ontology/vivo-ufl/>
PREFIX vcard:    <http://www.w3.org/2006/vcard/ns#>
PREFIX vitro:    <http://vitro.mannlib.cornell.edu/ns/vitro/public#>
PREFIX vivo:     <http://vivoweb.org/ontology/core#>
PREFIX scires:   <http://vivoweb.org/ontology/scientific-research#>
"

#---------------------------------------------------------------------
# Get the orgs, their names, and parent-child relationships

query <- "SELECT ?x (SAMPLE(DISTINCT(?xlabel)) AS ?labelx) ?y (SAMPLE(DISTINCT(?ylabel)) AS ?labely)
WHERE {
?x a foaf:Organization .
?x a ufVivo:UFEntity .
?x rdfs:label ?xlabel .
OPTIONAL {?x obo:BFO_0000051 ?y . ?y rdfs:label ?ylabel .} # x has part y
OPTIONAL {?x vivo:hasSuccessorOrganization ?successor .}
FILTER (!bound(?successor))
}
GROUP BY ?x ?y
ORDER BY ?x ?y"

q <- paste(prefix, query, sep='\n')
sparql <- SPARQL(url= endpoint, query= q)
results <- sparql$results

# construct the network object.  Result[,1] contains URIs of parent
# orgs.  Result[,3] contains URIs of child orgs. Make a two column
# matrix where each row is a parent-child relationship.  Remove rows
# where there is no child.  The result is an edgelist for the graph

nodes <- as.factor(c(results[,1],results[,3]))   # the nodes
m <- matrix(as.integer(nodes),length(nodes)/2,2) # the edge list
m2 <- m[!is.na(m[,2]),]                          # remove missing 
net<-network(m2,directed=F)                      # construct the network

# ----------------------------------------------------------------------
# Get the names of each node and assign them as vertex attributes

name.list <- NULL
for (i in seq(1,nrow(results))){
  uri <- results[i,1]
  name <- results[i,2]
  if (!is.na(uri) && !is.na(name)) name.list[[uri]] <- fix.vivo.label(name)
  uri <- results[i,3]
  name <- results[i,4]
  if (!is.na(uri) && !is.na(name)) name.list[[uri]] <- fix.vivo.label(name)
  }
  
# for each node, find its name by using its URI

node.names <- NULL
node.uris <- NULL
for (i in seq(1,length(levels(nodes)))){
  uri <- levels(nodes)[i]
  node.uris <- c(node.uris,uri)
  nm <- name.list[[uri]] 
  node.names <- c(node.names,nm)
  }
set.vertex.attribute(net,"vertex.names",node.names)

# ------------------------------------------------------------------
# for each node, add type

query <- "SELECT ?x $type
WHERE {
?x a foaf:Organization .
?x a ufVivo:UFEntity .
?x rdf:type ?type
}
"

q <- paste(prefix, query, sep='\n')
sparql <- SPARQL(url= endpoint, query= q)
types <- sparql$results

ok.types <- c("University","College","Department","Institute","Center",
    "ExtensionUnit","Division","Program","AdministrativeUnit","Hospital",
    "Museum","School","Laboratory","Library")
org.types <- NULL
for (i in seq(1,nrow(types))){
  type <- fix.vivo.type(types[i,2])
  if (type %in% ok.types) org.types[[types[i,1]]] <- type
  }

node.types <- NULL
for (i in seq(1,length(levels(nodes)))){
  uri <- levels(nodes)[i]
  if (uri %in% names(org.types)) type <- org.types[[uri]]
  else type <- "Unknown"
  node.types <- c(node.types,type)
  }
set.vertex.attribute(net,"types",node.types)

# ------------------------------------------------------------------------
# Plot the network

pdf(height=72,width=72,file="uf organization.pdf") # six foot square PDF
#palette(rainbow(length(table(node.types))))
#palette(heat.colors(length(table(node.types))))
palette(topo.colors(length(table(node.types))))
#palette(gray(seq(0,.9,len=length(table(node.types)))))
plot(net,displaylabels=TRUE,layout.par=list("niter"=1000),label.pos=6,vertex.cex=0.5,vertex.col=as.numeric(as.factor(get.vertex.attribute(net,"types"))))
dev.off()

# -------------------------------------------------------------------------
# Save the data
report <- cbind(node.uris,node.names,node.types)
write.csv(report,file="org-query.csv")

Sys.time()
# Done



