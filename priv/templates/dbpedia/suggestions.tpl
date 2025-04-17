{% if suggestions %}
    <ul>
        {% for rdf in suggestions|exclude_property:"uri":id.o.hasreference|slice:[,3] %}
            {% include "reference/suggestion/list-item-dbpedia-suggestion.tpl" r=m.rdf[rdf] add %}
        {% endfor %}
    </ul>
{% endif %}
