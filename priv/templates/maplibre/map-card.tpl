{% with m.rsc[q.id].id as id %}
    <a href={{ id.page_url }} class="c-timeline__link">
        <div class="c-timeline__item c-timeline__map-card">
            <div class="c-timeline-card">
                <div class="u-d-flex u-flex-justify-between">
                    <span class="c-timeline__date">
                        {% if id.category.name == "event" %}
                            {% if id.date_start %}{{ id.date_start|date:"j F Y" }}{% endif %}
                            {% if id.date_end %} - {{ id.date_end|date:"j F Y" }}{% endif %}
                        {% else %}
                            {{ id.publication_start|date:"j F Y" }}
                        {% endif %}
                    </span>
                    {% block card_category %}

                        {% if id.category.name == "event"  %}
                            <span class="c-timeline__category">
                                {{ m.rsc[id.category.id].title }}
                                {% if id.address_city  %}
                                    <i class="icon--location"></i>
                                    {{ id.address_city }}
                                {% endif %}
                            </span>
                        {% else %}
                            <span class="c-timeline__category">{{ m.rsc[id.category.id].title }}</span>
                        {% endif %}

                    {% endblock %}
                </div>
                <hr class="c-timeline__separator"></hr>

                {% block card_content %}
                    {% if id.category.name == "event" and id.depiction|is_visible %}
                        {% image id.depiction class="c-timeline-card__image" %}
                        <h3 class="c-timeline-card-title">{{ id.title }}</h3>
                        <p class="c-timeline__excerpt">{{ id.summary|truncate:100 }}</p>
                    {% else %}
                        {% with
                            m.remarks.for_contribution[id].latest,
                            m.remarks[id].topic
                        as
                            latest_remark,
                            topic
                        %}
                            {% with id.o.author[1]|default:id.creator_id as creator %}
                                <strong>{% include "person/person-title.tpl" id=creator %}</strong>
                            {% endwith %}

                            {% with id.og_title|if:id:id.about as topic %}
                            {% if topic.og_title %}
                                <div class="list-item-kg__content--og">
                                    <h3 class="list-item-kg__content__title"><span>{{ id.title|striptags|truncate:100 }}</span></h3>
                                    {% include "og/og-data.tpl" nolink id=topic %}
                                </div>
                            {% else %}
                                <div class="list-item-kg__content">
                                    <h3>
                                        {{ ((id.category.name == "remark")|if:id.body:id.title)|striptags|truncate:100 }}
                                    </h3>
                                </div>
                            {% endif %}

                            {% if latest_remark and (not topic.og_title) %}
                                <div class="remark-item__wrapper remark-item__list-item">
                                    <header class="person-author">
                                        {% if not latest_remark.creator_id.is_published %}
                                            {% include "avatar/avatar.tpl" id=m.rsc.custom_avatar_fallback.id fallback_rsc_id=m.rsc.custom_avatar_fallback.id %}
                                        {% else %}
                                            {% include "avatar/avatar.tpl" id=latest_remark.creator_id fallback_rsc_id=m.rsc.custom_avatar_fallback.id %}
                                        {% endif %}
                                    </header>
                                    <div class="remark-item__body">
                                        {{ latest_remark.body|striptags|truncate:90 }}
                                    </div>
                                </div>
                            {% endif %}
                            {% endwith %}

                        {% endwith %}
                    {% endif %}
                    
                {% endblock %}
            </div>
        </div>
    </a>
{% endwith %} 