{% extends "timeline/_timeline-card.tpl" %}

{% block card_category %}
    <span class="c-timeline__category">
        {{ id.category.name }}
        {% if id.address_city  %}
            <i class="icon--location"></i>
            {{ id.address_city }}
        {% endif %}
    </span>
{% endblock %}
