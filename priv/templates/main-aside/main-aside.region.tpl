{% if kc_groups|make_list as contentgroups %}
    {% if m.search.paged[{query content_group=contentgroups sort="-rsc.created" cat="contribution" pagelen=4 page=q.page}] as result %}
        <h3 class="bordered-title">Bijdragen binnen de kennisgroepen in {{ id.address_city|truncate:35 }}</h3>

        {% include "list/list.tpl" list_id="list--content" class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" id=id noresults=" " %}
    {% endif %}

    {% if m.search.paged[{query upcoming content_group=contentgroups sort="-rsc.pivot_date_start" cat="event" pagelen=4 page=q.page}] as result %}
        <h3 class="bordered-title">Meetups in {{ id.address_city|truncate:35 }}</h3>

        {% include "list/list.tpl" list_id="list--upcoming" class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" id=id noresults=" " %}
    {% endif %}

    <h3 class="bordered-title">Kennisgroepen in {{ id.address_city|truncate:35 }}</h3>
    {% include "list/list.tpl" list_id="list--kc_groups" items=kc_groups %}
{% endif %}

{% if m.search[{query
    cat="person"
    hasobject=[id, "hasregion"]
    sort="-pivot.kenniscloud_users.has_depiction"
    pagelen=100000
}] as result %}
    <h3 class="bordered-title region-members">Wij doen mee in {{ id.address_city|truncate:35 }} en omstreken</h3>

    <div class="home-members__list">
        {% for r in result %}
            {% include "list/list-item-person-small.tpl" id=r summary is_librarian=0 %}
        {% endfor %}
    </div>
{% endif %}
