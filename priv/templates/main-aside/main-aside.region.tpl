{% if m.search.paged[{query region_content_groups=id sort="-rsc.created" cat="contribution" pagelen=4}] as result %}
    <h3 class="bordered-title">Bijdragen binnen de kennisgroepen in {{ id.address_city|truncate:35 }}</h3>

    {% include "list/list.tpl" list_id="list--content" class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" id=id noresults=" " %}
{% endif %}

{% if m.search.paged[{query upcoming region_content_groups=id sort="-rsc.pivot_date_start" cat="event" pagelen=4}] as result %}
    <h3 class="bordered-title">Meetups in {{ id.address_city|truncate:35 }}</h3>

    {% include "list/list.tpl" list_id="list--upcoming" class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" id=id noresults=" " %}
{% endif %}

{% if m.search[{query hasobject=[id,'hasregion'] cat="acl_collaboration_group" pagelen=9}] as result %}
    <h3 class="bordered-title">Kennisgroepen in {{ id.address_city|truncate:35 }}</h3>
    {% include "list/list.tpl" list_id="list--kc_groups" items=result %}
{% endif %}

{% if m.search[{query
    cat="person"
    hasobject=[id, "hasregion"]
    sort="-pivot.kenniscloud_users.has_depiction"
    pagelen=30
}] as result %}
    <h3 class="bordered-title region-members">Wij doen mee in {{ id.address_city|truncate:35 }} en omstreken</h3>

    {% include
        "list/list.tpl"
        items=result
        list_template="list/list-item-person-small.tpl"
        class="person"
        extraClasses="home-members__list"
    %}
{% endif %}
