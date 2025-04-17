{% extends "page-actions/page-actions.tpl" %}

{% block more_page_actions %}
    {% if id.name == "page_kennisgroepen" %}
        {% include "page-actions/page-action-create-group.tpl" id=id tabs_enable=["new"] %}
    {% else %}
        {# voeg iets toe aan een gewone collectie? #}
    {% endif %}
{% endblock %}
