{% extends "acl_collaboration_group/additional-content.tpl" %}

{% block content_bottom %}
    <div class="main-container--related">
        <h2>{_ Frequently Asked Questions _}:</h2>
        <ul class="">
            {% for faq in id.o.hasextra_faq|is_visible %}
                {% catinclude "list/list-item.tpl" faq %}
            {% endfor %}
        </ul>
    </div>
{% endblock %}
