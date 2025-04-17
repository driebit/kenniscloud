{% extends "acl_collaboration_group/additional-content.tpl" %}

{% block content_bottom %}
    <div class="main-container--related">
        <h2>{_ Additional Documents _}:</h2>
        <ul class="list">
            {% for doc in id.o.hasextra_doc|is_visible %}
                <li class="list__item {{ extraClasses }}">
                    <a href="{{ doc.page_url }}">
                            <div class="list__item__image">
                                {% image doc.id mediaclass="list-image" alt="" title="" crop=dep_rsc.crop_center %}
                            </div>
                            <div class="u-padding-1">
                                <h3>
                                    {% if doc.short_title %}
                                        {{ doc.short_title|truncate:40 }}
                                    {% else %}
                                        {{ doc.title|truncate:40 }}
                                    {% endif %}
                                    ({% if m.media[doc].mime == "application/pdf" %}pdf{% else %}document{% endif %}&nbsp;-&nbsp;{{ m.media[doc].size|filesizeformat }})
                                </h3>
                            </div>
                    </a>
                </li>
            {% endfor %}
        </ul>
    </div>
{% endblock %}
