{% with
    m.remarks.for_contribution[id].latest,
    m.rsc[id].s.relation|first,
    m.remarks[id].topic
  as
    latest_remark,
    first_relation,
    topic
%}
{% with ((id.category.name == "tip")|if:first_relation:((id.category.name == "remark")|if:topic:0)|default:id) as ref %}
{% with id.o.hasbanner[1]|default:id.depiction.id as dep %}
<li class='list-item-kg {% if latest_remark %}has-remark{% endif %}' style="background-image: url('{% image_url dep mediaclass='list-image' crop=dep.crop_center %}'); background-repeat: no-repeat; background-size: cover;{% if id.is_a.contribution %}overflow: hidden;{% endif %}">
    {% if id.category.name == "remark" %}
        <div class="list-item-kg__remark">
            <i class="icon--comment"></i>
        </div>
    {% endif %}
    <a href="{{ ref.page_url }}">
        {% if id.status_label %} 
            <div class="list-item-kg-contribution__lvl">{{ id.status_label|translate }}</div>
        {% endif %}
        <div {% if dep %}class="list-item-kg-contribution__content"{% endif %}>
            <div class="list-item-kg__top">
                {% block list_author %}
                <div class="list-item-kg__author">
                    {% with id.o.author[1]|default:id.creator_id as creator %}
                        {% include "list/list-item-person-small.tpl" id=creator context_rsc=id class="person-author" nolink="true" %}
                    {% endwith %}
                </div>
                {% endblock %}

                {% include "meta/remarks.tpl" nolink="true" %}
                {% if id.is_featured %}
                    <i class="icon--pinned"></i>
                {% endif %}
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
                        {% include "list/list-item-person-small.tpl" id=latest_remark.creator_id context_rsc=id class="person-author" nolink="true" %}
                    </header>
                    <div class="remark-item__body">
                        {{ latest_remark.body|striptags|truncate:90 }}
                    </div>
                </div>
            {% endif %}
            {% endwith %}

            {% block list_meta %}
                <div class="list-item-kg__meta">
                    <time datetime="{{ id.publication_start|date:"Y-F-jTH:i" }}"><span>{{ id.publication_start|date:"j-m-y" }}</span><span>{{ id.publication_start|date:"j F Y" }}</span></time>

                    {% include "meta/likes.tpl" nolink="true" %}
                    {% include "meta/task-status.tpl" %}

                </div>
            {% endblock %}
        </div>
    </a>
</li>
{% endwith %}
{% endwith %}
{% endwith %}
