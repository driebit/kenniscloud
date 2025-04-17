{% extends "acl_collaboration_group/additional-content.tpl" %}

{% block content_bottom %}
<div class="main-container--related">
    <h2>{_ Additional Images _}:</h2>
    <ul class="list">
        {% for image in id.o.hasextra_img|is_visible %}
            <a href="{{ image.page_url }}" alt="{{ image.title }}">
                {% image image.id mediaclass="list-image" %}
                <div class="list__item__title">
                    <h4>
                        {% if image.short_title %}
                            {{ image.short_title|truncate:40 }}
                        {% else %}
                            {{ image.title|truncate:40 }}
                        {% endif %}
                    </h4>
                </div>
            </a>
        {% endfor %}
    </ul>
</div>
{% endblock %}
