{% extends "depiction/with_depiction.tpl" %}

{% block with_depiction %}

{% if id.is_visible %}

    <li class="list__item--person {{ extraClasses }}">

        {# {% include "page-actions/page-action-edit-thing.tpl" extraClasses="edit-button--list-item" id=id %} #}
        <a href="{{ id.page_url }}">
            <article>
                <div class="list__item__image">
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
