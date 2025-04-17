{% extends "depiction/with_depiction.tpl" %}

{% block with_depiction %}

{% if id.is_visible %}

    <li class="list__item {{ extraClasses }}">

        <a href="{{ id.page_url }}">
            <article>
                <div class="list__item__image">
                    {% image dep_rsc.id mediaclass="list-image" alt="" title="" crop=dep_rsc.crop_center %}
                </div>
                <div class="list__item__title">
                	{% include "category-of/category-of.tpl" nolink="true" %}

                	<h3>
                        {% if id.short_title %}
                            {{ id.short_title }}
                        {% else %}
                            {{ id.title }}
                        {% endif %}
                    </h3>
                </div>
                <div class="list__item__summary">
					{{ id|summary:100 }}
                </div>

                {% if id|is_a:event and id.date_start|date:"Y" %}
                    <time datetime="{{ id.date_start|date:"Y-F-jTH:i" }}">
                        <i class="icon--calendar"></i>
                        {% include "meta/date.tpl" id=id %}
                    </time>
                {% endif %}

            </article>
        </a>
    </li>

{% endif %}

{% endblock %}
