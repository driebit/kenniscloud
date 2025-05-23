{% extends "depiction/with_depiction.tpl" %}

{% block with_depiction %}
    {% with image_class|default:"avatar__image" as image_class %}
        {% if dep_rsc %}
            <div class="avatar__wrapper">
                {% image dep_rsc.id mediaclass="avatar" alt=dep_rsc.title class=image_class %}
            </div>
        {% endif %}
    {% endwith %}
{% endblock %}
