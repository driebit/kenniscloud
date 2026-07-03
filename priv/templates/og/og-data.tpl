{# TODO ZOTONIC1: check this if/when the integration with bibliotheek.nl is fixed #}
{% if id.og_title|default:id.title as title %}

{% if nolink %}
    <div class="og-data">
{% else %}
    <a href="{{ id.website }}" target="_blank" class="og-data">
{% endif %}

    {% if id.og_image|is_defined %}
        <div class="og-data__image">
            <img src="{{ id.og_image }}" alt="">
        </div>
    {% endif %}

    <div class="og-data__content {% if not id.og_image|is_defined %}-no-image{% endif %}">
        <small>{{ id.website|kc_domain_name_from_url }}</small>

        {% if nolink %}
            <h3>{{ title|truncate:40 }}</h3>
        {% else %}

            {% if title|match:".pdf" %}
                <h3 class="break-word">{{ title }}</h3>
            {% else %}
                <h3>{{ title }}</h3>
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
