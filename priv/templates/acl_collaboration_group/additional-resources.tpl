{% extends "acl_collaboration_group/additional-content.tpl" %}

{% block content_bottom %}
    <div class="main-container--related">
        <h2>{_ External resources _}:</h2>
        <ul class="list">
            {% for rsc in id.o.hasextra_rsc|is_visible %}
                {% catinclude "list/list-item.tpl" rsc %}
            {% endfor %}
        </ul>
    </div>
{% endblock %}
