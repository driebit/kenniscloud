{% extends "depiction/with_depiction.tpl" %}

{% block with_depiction %}

{% with collaboration_groups|default:(id.s.hasregion|is_a:"acl_collaboration_group") as collaboration_groups %}

{% if id.is_visible %}

    <li class="list__item {{ extraClasses }}">
        <a href="{{ id.page_url }}">
            <article>
                <div class="list__item__image">
                    {% image dep_rsc.id mediaclass="list-image" alt="" title="" crop=dep_rsc.crop_center %}
                </div>
                <div class="list__item__content">
                    <div class="list__item__title">
                        {% include "category-of/category-of.tpl" nolink="true" %}

                        <h3>
                            {% if id.short_title %}
                                {{ id.short_title|truncate:40 }}
                            {% else %}
                                {{ id.title|truncate:40 }}
                            {% endif %}
                        </h3>
                    </div>

                    {% include "keywords/keywords.tpl" nolink="true" slice_items=5 %}

                    {% with m.search[{query content_group=collaboration_groups|make_list sort="-rsc.created" cat=["reference", "contribution", "event"] pagelen=10000000 }] as contributions %}
                    <div class="list__item__meta">
                        <p>Aantal bijdragen <b>{{ contributions|length }}</b></p>
                        <p>Aantal kennisgroepen <b>{{ collaboration_groups|length }}</b></p>
                    </div>
                    {% endwith %}
                </div>
            </article>
        </a>
    </li>

{% endif %}

{% endwith %}

{% endblock %}
