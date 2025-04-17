{% for person in result|slice:show %}
    {% include "list/list-item-person-small.tpl" id=person %}
{% endfor %}
