{% extends "timeline/_timeline-card.tpl" %}

{% block card_time_category %}
    <span class="c-timeline__date">
        {% if id.date_start %}
            {{ id.date_start|date:"j F Y" }}
            {% if id.date_end %}
                - {{ id.date_end|date:"j F Y" }}
            {% endif %}
        {% else %}
            {{ id.publication_start|date:"j F Y" }}
        {% endif %}
    </span>
    <span class="c-timeline__category">
        {{ id.category.name }}
        {% if id.address_city  %}
            <i class="icon--location"></i>
            {{ id.address_city }}
        {% endif %}
    </span>
{% endblock %}
