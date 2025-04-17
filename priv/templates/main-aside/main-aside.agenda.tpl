{% with m.search[{query upcoming sort="rsc.pivot_date_start" cat="event" }] as upcoming_events %}
    <aside class="main-aside">

    {% if upcoming_events %}
    	<h3 class="bordered-title">Aankomende Meetups</h3>
        {% include "list/list.tpl" items=upcoming_events list_id="list--upcoming" class="list--kg" list_template="list/list-item-kg.tpl" hide_showmore_button hide_showall_button extraClasses="" id=id %}
	{% endif %}

    {% if m.search.paged[{query sort="-rsc.pivot_date_start" cat="event" pagelen=12 page=q.page }] as archived_events %}
		<h3 class="bordered-title">Archief</h3>
        {% include "list/list.tpl" items=archived_events exclude=upcoming_events|make_list list_id="list--content" class="list--kg" list_template="list/list-item-kg.tpl" extraClasses="" id=id noresults=" " hide_showall_button %}
    {% endif %}

    </aside>
{% endwith %}
