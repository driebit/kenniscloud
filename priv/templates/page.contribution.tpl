{% extends "page.event.tpl" %}

{% block og_data %}
    {% if id.o.about[1] as ref %}
        {% if ref.og_image|default:ref.o.depiction[1] as depiction %}
            {% catinclude "media/media.tpl" depiction %}
        {% endif %}
        
        {% include "og/og-data.tpl" id=ref.id %}
    {% endif %}
{% endblock %}

{% block page_body %}
    <div class="page-body">
    	{{ id.body|show_media }}

        {% if id.status_label != "Closed" %}
            {% include "library/add-references.tpl" %}
        {% endif %}
    </div>
{% endblock %}

{% block content_right %}
    <aside class="page-aside">
        
        {% include "page-actions/page-action-edit-thing.tpl" %}

        {% include "contribution/task-actions.tpl" id=id dispatch_to='page' %}

        {% catinclude "top-aside/top-aside.tpl" id %}
    </aside>
{% endblock %}
