{% extends "base.tpl" %}

{% block body_class %}-no-bg t--contribution{% endblock %}

{% block content %}
<main id="main-content">
    {% catinclude "masthead/masthead.tpl" id %}

    <article class="main-content">
        {% include "summary/summary.tpl" id=id %}

        {% include "body/body.tpl" id=id %}

        {% include "blocks/blocks.tpl" id=id %}

        {% block attached_media %}
        {% include "attached-media/attached-media.tpl" id=id %}
        {% endblock %}
    </article>
</main>
{% endblock %}
