{% extends "depiction/with_depiction.tpl" %}

{% block with_depiction %}

{% if id.is_visible %}

    <li class="list__item {{ extraClasses }} u-margin-bottom-2">

        
        <a href="{{ id.page_url }}">
            <article>
                <div class="list__item__content--faq">
                    <div class="list__item__title list__item__short-title u-margin-none">
                        {% include "keywords/keywords.tpl" nolink %}
                        <h3>{{ id.title }}</h3>
                    </div>

                    <div class="list__item__summary">
                       <b>{{ id.summary|truncate:600 }}</b>
                    </div>
                </div>
            </article>
        </a>
    </li>

{% endif %}

{% endblock %}
