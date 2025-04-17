{% extends "page.tpl" %}

{% block body_class %}t--collection{% endblock %}

{% block content %}

    <main>
        <div class="page-header -allin">
            {% include "page-title/page-title.tpl" id=id title="Alles voor: "++""++id.title++"" %}
        </div>

        {% block page_filters %}{% endblock %}

        {% block content_bottom %}
            <div class="main-container--related">
                {% if q.id %}
                    <aside class="main-aside">
                        {% if q.direction == 'subject' %}
                            {% with m.search.paged[{query hassubject=[q.id, q.type]  pagelen=6 page=q.page}] as result %}
                                {% include "list/list.tpl" list_id="list--query" items=result extraClasses="" id=id %}
                            {% endwith %}
                        {% else %}
                            {% with m.search.paged[{query hasobject=[q.id, q.type] cat_exclude=['media'] pagelen=6 page=q.page}] as result %}
                                {% include "list/list.tpl" list_id="list--query" items=result extraClasses="" id=id %}
                            {% endwith %}
                        {% endif %}
                    </aside>
                {% endif %}
            </div>
        {% endblock %}

    </main>
    {% block page_after_content %}{% endblock %}
{% endblock %}
