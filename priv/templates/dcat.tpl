@prefix dataid: <http://dataid.dbpedia.org/ns/core#> .
@prefix dcat: <http://www.w3.org/ns/dcat#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix nde: <http://schemas.netwerkdigitaalerfgoed.nl/registry#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix pav: <http://purl.org/pav> .
@prefix schema: <http://schema.org/> .
@prefix void: <http://rdfs.org/ns/void#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<{% url dcat absolute_url %}> a dcat:Catalog ;
    dcat:dataset {% for dataset in m.search[{query cat="dcat_dataset" is_published}] %}<{{ dataset.page_url_abs }}>{% if not forloop.last %}, {% endif %}{% endfor %} .

{% for id in m.search[{query cat="dcat_dataset" is_published='true'}] %}
    {% with m.rsc[id], m.creative_commons[r.rights].language_url as r, license_url %}
<{{ r.page_url_abs }}> a dcat:Dataset ;
    dcterms:identifier {{ id }} ;
    dcterms:title "{{ r.title }}" ;
    dcterms:description "{{ r.description }}" ;
    dcterms:issued "{{ r.publication_start | date:"Y-m-d" }}"^^xsd:date ;
    dcterms:modified "{{ r.modified | date:"Y-m-d" }}"^^xsd:date ;
    dcterms:license <{{ license_url }}> ;
    dcterms:landingPage <{{  r.page_url_abs }}> ;
    dcat:distribution _:dataset-{{ id }}-hdt .

_:dataset-{{ id }}-hdt
    a dcat:Distribution ;
    dcterms:title "HDT-download"@nl ;
    dcterms:description "HDT-download van {{ id.title }}"@nl ;
    dcat:downloadURL </hdt/{{ id.name }}.hdt> ;
    dcat:mediaType "application/vnd.hdt" .
    {% endwith %}
{% endfor %}
