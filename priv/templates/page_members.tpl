{% extends "base.tpl" %}

{% block body_class %}t--knowledge-group{% endblock %}

{% block content %}

    {% include "masthead/masthead.tpl" id=id %}

    <main>
        <div class="kg-intro">
            <div class="kg-intro__content">
                {% catinclude "category-of/category-of.tpl" id rsc_id=id %}

                <h1 class="page-title">Leden van de kennisgroep</h1>
                <h2>{{ id.title }}</h2>
                <p class="summary">Een overzicht van alle leden die participeren in de kennisgroep.</p>
                <a href="{{ id.page_url }}" class="btn--primary">Terug naar {{ id.title }}</a>
            </div>
        </div>

        <div class="kg-contributions">
            <ul class="list">
                {% for member in id|kc_collaboration_group_members %}
                    {% include "list/list-item.person.tpl" id=member %}
                {% endfor %}
            </ul>
        </div>
    </main>
{% endblock %}
