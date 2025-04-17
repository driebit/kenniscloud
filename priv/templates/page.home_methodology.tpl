{% extends "base.tpl" %}

{% block body_class %}t--methodology-homepage -no-bg{% endblock %}

{% block content %}
    {% if id.o.hasbanner|default:id.o.depiction[1] as masthead %}
        <main id="main-content">
            <div class="methodology-masthead">
                <div class="methodology-masthead__bg" style="background-image: url({% image_url masthead.id mediaclass="masthead" %});"></div>
                <div class="methodology-container">
                    {% include "page-title/page-title.tpl" id=id %}
                </div>
            </div>
            <div class="methodology-container">
                <div class="methodology-container__body">{{ id.body|show_media }}</div>
                {% if id.o.relation[1] as person %}
                    <aside class="methodology-container__relation">
                        <section class="methodology-container__relation__details">
                            <img class="methodology-container__relation__photo" src="{% image_url person.id %}" alt="Profielfoto van {{ person.title }}">
                            <hgroup>
                                <h3>{{ person.title|truncate:40 }}</h3>
                            </hgroup>
                        </section>
                        <section class="methodology-container__relation__summary">
                            <p>{{ id.summary }}</p>
                            <a href="mailto:{{ person.email }}" class="btn btn--primary">Stuur een email</a>
                        </section>
                    </aside>
                {% endif %}
            </div>
        </main>
    {% endif %}
{% endblock %}



