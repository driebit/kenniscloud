{% for oid in id.o.author %}
    {% catinclude "pivot/_title.tpl" oid %}
{% endfor %}

{% for z_language in id.language|default:[z_language] %}
    {{ id.summary }} {{ id.body }} {{ id.og_description }}
{% endfor %}

{% if id.is_a.person %}
    {{ id.function }}

    {% for expertise in m.rsc[`collection_expert_predicates`].o.haspart %}
        {% if id.s[expertise] %}
            {{ expertise.title }}
        {% endif %}
    {% endfor %}
{% endif %}


{{ id.address_street_1 }}
{{ id.address_street_2 }}
{{ id.address_city }}
{{ id.address_state }}
{{ id.address_postcode }}
{{ id.address_country }}
{% if id.address_country and m.modules.active.mod_l10n %}
    {% for z_language in id.language|default:[z_language] %}
        {{ m.l10n.country_name[id.address_country] }}
    {% endfor %}
{% endif %}

{{ id.website }}
{{ id.facebook }}
{{ id.twitter }}
{{ id.linkedin }}
{{ id.has_mentilink }}
{{ id.has_opnformlink }}

{% if not id.is_a.person %}
    {{ id.publication_start }}
    {{ id.date_start }} {{ id.date_end }}
    {{ id.created }}
{% endif %}

{% if id.medium as medium %}
    {% if medium.filename as filename %}
        {{ filename|split:"/"|last }}
    {% endif %}
{% endif %}
{% if id.og_image as og_image_path %}
    {{ og_image_path|split:"/"|last }}
{% endif %}
