{% extends "page.tpl" %}

{% block body_class %}t--collection{% endblock %}

{% block content %}

    <main>
        <div class="page-header">
            <div class="page-intro">

                {% include "page-title/page-title.tpl" %}

                {% include "subtitle/subtitle.tpl" %}

                {% include "summary/summary.tpl" %}

                {% include "page-actions/page-action-edit-thing.tpl" %}
            </div>

            {% block content_right %}
                <aside class="page-aside">
                    {% catinclude "top-aside/top-aside.tpl" id %}
                </aside>
            {% endblock %}
        </div>

        {% block page_filters %}{% endblock %}

        {% block content_bottom %}
            <div class="main-container--related">
                {% catinclude "main-aside/main-aside.tpl" id %}
            </div>
        {% endblock %}

    </main>
    {% block page_after_content %}{% endblock %}
{% endblock %}
