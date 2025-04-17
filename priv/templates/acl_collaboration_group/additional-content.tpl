{% extends "base.tpl" %}

{% block body_class %}t--knowledge-group{% endblock %}

{% block content %}
{% with id as collab_group %}

    {% catinclude "masthead/masthead.tpl" id %}

    <main>
        <div class="kg-intro">
            <div class="kg-intro__content">
                {% include "page-title/page-title.tpl" id=id %}

                <div class="u-d-flex u-flex-justify-between">
                    {% include "page-actions/page-action-edit-thing.tpl" %}
                    <a class="btn--primary" href={{ id.page_url }}>{_ Back _}</a>
                </div>

            </div>
        </div>
            
        {% block content_bottom %}
        {% endblock %}
    </main>

{% endwith %}
{% endblock %}