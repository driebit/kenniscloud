{% extends "depiction/with_depiction.tpl" %}

{% block with_depiction %}

{% if id.is_visible %}

    <li class="list__item--person {{ extraClasses }}">

        <a href="{{ id.page_url }}">
            <article>
                <div class="list__item__image" style="background-image: url({% image_url id.o.hasbanner[1].depiction.id mediaclass='masthead' crop=dep.crop_center %}); background-size: cover;">
                    {% image dep_rsc.id mediaclass="avatar" alt="" title="" crop=dep_rsc.crop_center %}
                </div>
                <div class="list__item__content">
                    <div class="list__item__title {% if id.o.hasusergroup.name == "acl_user_group_community_librarian" %} is-cl{% endif %}">

                    	{% include "category-of/category-of.tpl" nolink="true" %}

                    	<h3>
                            {% if id.short_title %}
                                {{ id.short_title|truncate:40 }}
                            {% else %}
                                {{ id.title|truncate:40 }}
                            {% endif %}
                        </h3>
                    </div>
                    <div class="list__item__summary">
    					{{ id|summary:100 }}
                    </div>
                </div>
            </article>
        </a>
    </li>

{% endif %}

{% endblock %}
