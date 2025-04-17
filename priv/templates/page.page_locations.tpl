{% extends "page.tpl" %}

{% block content %}

    <main>
        <div class="page-header">
            <div class="page-intro">
                {% include "page-title/page-title.tpl" %}
            </div>

            {% block content_right %}{% endblock %}
        </div>

        {% block page_filters %}{% endblock %}

        {% block content_bottom %}{% endblock %}

    </main>

    {% block page_after_content %}
        {% include "cta/cta.page_subjects.tpl" %}
    {% endblock %}
{% endblock %}
