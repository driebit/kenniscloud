{% if r.uri|match:"^http://nl.dbpedia.org/" %}
    {% include "list/list-item-small-dbpedia.tpl" %}
{% elseif r.uri|match:"^http://data.bibliotheek.nl/" %}
    {% include "list/list-item-small-library.tpl" %}
{% endif %}
