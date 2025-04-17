{% extends "timeline/_timeline-card.tpl" %}

{% block card_category %}
    <span class="c-timeline__category">{{ m.rsc[id.category.id].title }}</span>
{% endblock %}

{% block card_content %}
    {% with
        m.remarks.for_contribution[id].latest,
        m.remarks[id].topic
    as
        latest_remark,
        topic
    %}
        <div class="list-item-kg__top">
            {% block list_author %}
            <div class="list-item-kg__author">
                {% with id.o.author[1]|default:id.creator_id as creator %}
                    {% include "list/list-item-person-small.tpl" id=creator context_rsc=id class="person-author" nolink="true" %}
                {% endwith %}
            </div>
            {% endblock %}
        </div>

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
{% endblock %}
