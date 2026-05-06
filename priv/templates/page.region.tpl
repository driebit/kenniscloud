{% extends "base.tpl" %}

{% block body_class %}t--region{% endblock %}

{% block content %}

    {% catinclude "masthead/masthead.tpl" id %}
    <main class="o-main-container">
        <div class="kg-intro">
            <div class="kg-intro__content">
                {% catinclude "category-of/category-of.tpl" id %}

                {% include "page-title/page-title.tpl" id=id %}

                {% include "summary/summary.tpl" id=id %}

                {% catinclude "keywords/keywords.tpl" id %}

                {% include "page-actions/page-action-edit-thing.tpl" %}
            </div>

            {% catinclude "top-aside/top-aside.tpl" id %}
        </div>

        <div class="kg-contributions o-main-container">
            {% catinclude "main-aside/main-aside.tpl" id %}
        </div>
    </main>

{% endblock %}
