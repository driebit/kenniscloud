{% extends "page_ginger_edit.tpl" %}

{% block header_text %}
    <h1 class="page-title">{_ Edit _}: {% if id.title %} {{ id.title }}{% else %}{{ id.category.id.title }}{% endif %}</h1>

    {% include "contribution/task-actions.tpl" id=id dispatch_to='ginger_edit' %}
{% endblock %}
