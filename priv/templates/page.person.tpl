{% extends "base.tpl" %}
{% block body_class_category %} {% endblock %}
{% block body_class %}t--person{% endblock %}

{% block content %}
{% catinclude "masthead/masthead.tpl" id %}

<div class="main-container person-info">
    <div class="person-info__contact">
        <h3 class="bordered-title">Biografie en interessegebieden</h3>
        {% if id.function %}
        <p class="person-info__contact__line"><strong>Functie</strong> <span>{{ id.function }}</span></p>
        {% endif %}

        {% if id.address_city %}
        <p class="person-info__contact__line"><strong>Woonplaats</strong> <span>{{ id.address_city|truncate:30 }}</span></p>
        {% endif %}

        {% with m.rsc[`collection_expert_predicates`].o.haspart as expertpredicates %}
        {% for expertise in expertpredicates %}
        {% if id.s[expertise] as projects %}
        <div class="person-info__contact__line"><strong>{{ expertise.title }} voor project</strong>
            <div>
                {% for project in projects %}
                <a href="{{ project.page_url }}" class="person-info__contact__line">{{ project.title }}</a>
                {% endfor %}
            </div>
        </div>
        {% endif %}
        {% endfor %}
        {% endwith %}

        {% if id.o.hasregion as regions %}
        <div class="person-info__contact__line"><strong>Regio's</strong>
            <div>
                {% for region in regions %}
                <a href="{{ region.page_url }}" class="person-info__contact__line">{{ region.title }}</a>
                {% endfor %}
            </div>
        </div>
        {% endif %}

        {% if id.o.hasusergroup[1].id == m.rsc.acl_user_group_community_librarian.id %}
        <p class="person-info__contact__line"><strong>Email</strong> <span><a href="mailto:{{ id.email }}">{{ id.email|truncate:40 }}</a></span></p>
        {% endif %}

        {% if id.s.hascollabmanager as collab_groups %}
        <div class="person-info__contact__line"><strong>Kennisgroepen (beheer)</strong>
            <div>
                {% for r in collab_groups %}
                <a href="{{ r.page_url }}">{{ r.title }}</a>{% if not forloop.last %}, {% endif %}
                {% endfor %}
            </div>
        </div>
        {% endif %}
        {% if id.s.hascollabmember as collab_groups %}
        <div class="person-info__contact__line"><strong>Kennisgroepen</strong>
            <div>
            {% for r in collab_groups %}
                <a href="{{ r.page_url }}">{{ r.title }}</a>{% if not forloop.last %}, {% endif %}
                {% endfor %}
            </div>
        </div>
        {% endif %}
            {% catinclude "page-actions/page-actions.tpl" id %}
        </div>
        <div class="person-info__summary">
            <h3 class="bordered-title">Over mijzelf</h3>
            {% include "summary/summary.tpl" %}
        </div>

    </div>

    {% catinclude "main-aside/main-aside.tpl" id %}
{% endblock %}
