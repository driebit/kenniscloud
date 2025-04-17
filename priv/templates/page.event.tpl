{% extends "page.tpl" %}

{% block page_actions %}
    {% include "page-actions/page-action-like.tpl" %}
{% endblock %}

{% block body_class %}t--contribution{% endblock %}

{% block content %}

    <main>
        <div class="page-container">
            <div class="page-left">
                <div class="page-intro page-intro--{{id.category.name}}">
                    {% spaceless %}
                        {% catinclude "category-of/category-of.tpl" id rsc_id=id.content_group_id %}
                        {% include "_hidden-resource.tpl" %}
                    {% endspaceless %}

                    {% block page_actions %}{% endblock %}

                    {% if id.date_start|date:"Y" %}
                        <time datetime="{{ id.date_start|date:"Y-F-jTH:i" }}">{{ id.date_start|date:"j.n.Y" }}</time>
                    {% endif %}

                    {% include "meta/task-status.tpl" extraClass="list-item-kg__task--white" %}

                    {% include "page-title/page-title.tpl" %}

                    {% catinclude "keywords/keywords.tpl" id %}

                    {% block og_data %}{% endblock %}

                    {% if id.o.hasbanner|default:id.depiction as depiction %}
                        {% if depiction.id.is_visible %}
                            {% catinclude "media/media.tpl" depiction.id %}
                        {% endif %}
                    {% endif %}

                    {% block documents %}
                        {% if id.o.hasdocument as documents %}
                            <ul class="page-intro__documents">
                            <h3>Documenten</h3>
                                {% for r in documents|is_visible %}
                                    <li>
                                        <span class="c-document-info">
                                            <a href="/document/{{ r.medium.filename }}" target="_blank">
                                                <i class="icon--download"></i><b>{{ r.title }}</b>
                                            </a>&nbsp;({% if m.media[r].mime == "application/pdf" %}pdf{% else %}document{% endif %}&nbsp;-&nbsp;{{ m.media[r].size|filesizeformat }})
                                        </span>
                                    </li>
                                {% endfor %}
                            </ul>
                        {% endif %}
                    {% endblock %}
                </div>
                <div class="page-under-intro">
                    {% block page_meta %}
                        <div class="page-meta">
                            {% include "meta/remarks.tpl" %}
                            {% include "meta/likes.tpl" %}
                            {% include "page-actions/page-action-flag.tpl" %}
                        </div>
                    {% endblock %}

                    <div class="page-under-intro__talk-along">
                    {% if not m.acl.user %}
                        <a href="{% url logon p={page id=id}|url %}" class="btn--primary">
                            Registreer of log in om te reageren
                        </a>
                    {% else %}
                        <a href="#reacties" class="do_anchor btn--primary -is-anchor">
                            Direct reageren
                        </a>
                    {% endif %}
                    </div>

                    {% block page_summary %}
                        {% include "summary/summary.tpl" %}
                    {% endblock %}
                </div>

                {% block page_body %}
                    {% if id.body %}
                        <div class="page-body">
                            <h3 class="bordered-title">Over het programma</h3>
                            {% include "body/body.tpl" %}
                        </div>
                    {% endif %}
                {% endblock %}

                {% block page_media %}
                    {% if id.o.depiction %}
                        <div id="beeldverslag" class="page-media">
                            <h3 class="bordered-title">Beeldverslag</h3>
                            <div id="owl-carousel" items="{{ id.o.depiction|length }}" class="owl-carousel">
                                {% for r in id.o.depiction %}
                                    {% catinclude "meetup/_media.tpl" r position=forloop.counter total=id.o.depiction|length %}
                                {% endfor %}
                            </div>
                        </div>
                    {% endif %}
                {% endblock %}

                {% block remarks %}
                    <div id="reacties" class="page-remarks">
                        {% include "comments/comments.tpl" %}
                    </div>
                {% endblock %}
            </div>

            {% block content_right %}
                <aside class="page-aside">
                    {% include "page-actions/page-action-edit-thing.tpl" %}

                    {% catinclude "top-aside/top-aside.tpl" id %}
                </aside>
            {% endblock %}
        </div>

        {% block content_tips %}
            {% include "tips/tips.tpl" %}
        {% endblock %}

        {% block content_bottom %}
            <div class="main-container--related">
                {% catinclude "main-aside/main-aside.tpl" id %}
            </div>
        {% endblock %}

    </main>
{% endblock %}
