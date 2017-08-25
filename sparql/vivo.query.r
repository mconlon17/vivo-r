vivo.query <- function(query, endpoint= 'http://localhost:8080/vivo/api/sparqlQuery',
  email= 'vivo_root@school.edu', password= '*******', format="tsv",
  ns = c(
    "rdf","<http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
    "rdfs","<http://www.w3.org/2000/01/rdf-schema#>",
    "xsd","<http://www.w3.org/2001/XMLSchema#>",
    "owl","<http://www.w3.org/2002/07/owl#>",
    "vitro","<http://vitro.mannlib.cornell.edu/ns/vitro/0.7#>",
    "bibo","<http://purl.org/ontology/bibo/>",
    "event","<http://purl.org/NET/c4dm/event.owl#>",
    "foaf","<http://xmlns.com/foaf/0.1/>",
    "obo","<http://purl.obolibrary.org/obo/>",
    "skos","<http://www.w3.org/2004/02/skos/core#>",
    "uf","<http://vivo.school.edu/ontology/uf-extension#>",
    "vitrop","<http://vitro.mannlib.cornell.edu/ns/vitro/public#>",
    "vivo","<http://vivoweb.org/ontology/core>")
    ){
  # Given an endpoint url, a query, and username and password, call
  # A VIVO SPARQL API and return the requested triples
  SPARQL(endpoint,q,ns=ns,extra=list(email=email,password=password),format=format)
}
