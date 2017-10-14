all: prolog/rdf_owl/owl.pl

ontologies/owl.rdf: ontologies/owl-edit.ttl
	riot --output=rdfxml  $< > $@
ontologies/owlrdfs.rdf: ontologies/owl.rdf ontologies/rdfs.rdf
	riot --output=rdfxml $^  > $@

prolog/rdf_owl/owl.pl: ontologies/owlrdfs.rdf
	rdfs2pl owl $< > $@
