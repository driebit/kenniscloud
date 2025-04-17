{% if m.config.site.title.value %}
    <meta name="twitter:site" content="{{ m.config.site.title.value }}">
{% endif %}
{% if id %}
    <meta name="twitter:title" content="{{ id.title }}">
    <meta name="twitter:description" content="{{ id|summary:135 }}">

    {% with m.rsc.contribution.depiction as contribution_image %}
    {% with id.depiction as depiction_image %}
    {% with id.hasbanner as banner_image %}

    {% if (id|is_a:"contribution" and contribution_image) or (id|is_a:"reference" and contribution_image) %}
        <meta name="twitter:image" content="http://{{ m.site.hostname }}{% image_url contribution_image mediaclass="facebook-og" %}">
        <meta name="twitter:card" content="summary_large_image">
    {% elseif depiction_image %}
        <meta name="twitter:image" content="http://{{ m.site.hostname }}{% image_url depiction_image mediaclass="facebook-og" %}">
        <meta name="twitter:card" content="summary_large_image">
    {% elseif id|is_a:"event" and banner_image %}
        <meta name="twitter:image" content="http://{{ m.site.hostname }}{% image_url banner_image.id mediaclass="facebook-og" %}">
        <meta name="twitter:card" content="summary_large_image">
    {% else %}
        <meta name="twitter:card" content="summary">
    {% endif %}

    {% endwith %}
    {% endwith %}
    {% endwith %}

{% else %}
    {% block no_id %}
        <meta name="twitter:card" content="summary">
    {% endblock %}
{% endif %}
