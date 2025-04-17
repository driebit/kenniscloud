{% extends "base.tpl" %}
{# Note: in Zotonic 0.x this was originally part of mod_ginger_foundation #}

{% block body_class %}t--no-masthead{% endblock %}

{% block content %}
<main>
    <div class="foldout do_foldout">
        <article class="main-content">
            {% include "page-title/page-title.tpl" id=id title=_"Everything for:"++" "++id.title %}
        </article>
    </div>
    {% if id %}
    <aside class="main-aside">
        {% with q.type|default:"subject" as type %}
            {% if q.direction == 'subject' %}
                {% with m.search.paged[{query hassubject=[id, type] pagelen=6 page=q.page}] as result %}
                    {% include "list/list.tpl" list_id="list--query" items=result extraClasses="" id=id %}
                {% endwith %}
            {% else %}
                {% with m.search.paged[{query hasobject=[id, type] cat_exclude=['media'] pagelen=6 page=q.page}] as result %}
                    {% include "list/list.tpl" list_id="list--query" items=result extraClasses="" id=id %}
                {% endwith %}
            {% endif %}
        {% endwith %}
    </aside>
    {% endif %}
</main>
{% endblock %}
