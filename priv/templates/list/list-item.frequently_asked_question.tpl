{% extends "depiction/with_depiction.tpl" %}

{% block with_depiction %}

{% if id.is_visible %}

    <li class="list__item {{ extraClasses }} u-margin-bottom-2">

        <a href="{{ id.page_url }}">
            <article>
                <div class="list__item__content--faq">
                    <div class="list__item__title list__item__short-title u-margin-none">
                        <h3>
                            {% if id.short_title %}
                                {{ id.short_title|truncate:40 }}
                            {% else %}
                                {{ id.title|truncate:40 }}
                            {% endif %}
                        </h3>
                    </div>

                    <div class="list__item__summary">
                       <b>{{ id.summary }}</b>
                    </div>

                    <div class="list__item__summary">
                        {{ id.body }}
                    </div>

                    {% if id.is_a.event and id.date_start|date:"Y" %}
                        <time datetime="{{ id.date_start|date:"Y-F-jTH:i" }}">
                            <i class="icon--calendar"></i>
                            {% include "meta/date.tpl" id=id %}
                        </time>
                    {% endif %}

                </div>
            </article>
        </a>
    </li>

{% endif %}

{% endblock %}
