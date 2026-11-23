{% extends "base.tpl" %}

{% block body_class %}-no-bg{% endblock %}

{% block content %}
    <main class="o-main-container" id="main-content">
        {% catinclude "masthead/masthead.tpl" id %}

        <article class="main-content">
            {% include "summary/summary.tpl" id=id %}

            {% include "body/body.tpl" id=id %}

            {% include "_blocks.tpl" id=id %}
        </article>
    </main>
{% endblock %}
