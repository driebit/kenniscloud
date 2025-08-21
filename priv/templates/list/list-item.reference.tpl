{% if parent == "search_page" %}
    <li class="list__item {{ extraClasses }}">
        {% if not nolink %}
            <a href="{{ id.website }}" target="_blank" rel="noopener noreferrer">
        {% endif %}
            <article>
                <div class="list__item__image">
                    {% if id.og_image %}<img src="{{ id.og_image }}" alt="">{% endif %}
                </div>
                <div class="list__item__content">
                    <div class="list__item__title">
                        {% include "category-of/category-of.tpl" nolink="true" %}

                        <h3>
                            {{ id.og_title|truncate:40 }}
                        </h3>
                    </div>
                    <div class="list__item__summary">
                        {{ id.website|kc_domain_name_from_url }}
                    </div>
                </div>
            </article>
        {% if not nolink %}
        </a>
        {% endif %}
    </li>
{% else %}
    {% if id.og_title %}

    {% if nolink %}
        <div class="c-card--reference">
    {% else %}
        <a href="{{ id.website }}" target="_blank" class="c-card--reference">
    {% endif %}

        {% if id.og_image|is_defined %}
            <div class="og-data__image">
                <img src="{{ id.og_image }}" alt="">
            </div>
        {% endif %}

        <div class="og-data__content {% if not id.og_image|is_defined %}-no-image{% endif %}">
            <small>{{ id.website|kc_domain_name_from_url }}</small>

            {% if nolink %}
                <h3>{{ id.og_title|truncate:40 }}</h3>
            {% else %}

                {% if id.og_title|match:".pdf" %}
                    <h3 class="break-word">{{ id.og_title }}</h3>
                {% else %}
                    <h3>{{ id.og_title }}</h3>
                {% endif %}

                <p>{{ id.og_description|truncate:300 }}</p>
            {% endif %}
        </div>

    {% if nolink %}
        </div>
    {% else %}
        </a>
    {% endif %}

    {% endif %}
{% endif %}
