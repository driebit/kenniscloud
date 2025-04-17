{% if id.date_start|date:"Y" %}
	<div class="page-aside__date">
		<time datetime="{{ id.date_start|date:"Y-F-jTH:i" }}">
            <i class="icon--calendar"></i>
			<span>{% include "meta/date.tpl" id=id %}</span>
		</time>
	</div>
{% endif %}

{% if id.address_city %}
	<div class="page-aside__location">
		<i class="icon--location"></i>
		<div>
			<span>{{ id.address_city|truncate:35 }}</span>
			<span>{{ id.address_title }}</span>
			<span>{{ id.address_street_1|truncate:35 }}</span>
		</div>
	</div>
{% endif %}

{% include "page-actions/page-action-rsvp.tpl" btn_class="btn--primary" btn_connect_text='Reserveer' btn_cancel_text='Annuleer reservering' %}

<div class="page-aside__crowd">
    <h3 class="bordered-title">Bekijk wie er komen</h3>

    <ul class="crowd-aside">
        <a href="/crowd/{{ id }}">
            <img src="/lib/images/Icon_Crowd.png" />
            <div class="btn--dark">Meetup crowd</div>
        </a>
    </ul>
</div>

<div class="page-aside__share">
    <h3 class="bordered-title">Delen</h3>
    {% include "share/share.tpl" %}
</div>

{% with id.o.author[1]|default:id.creator_id as creator %}
{% with m.rsc[id.content_group_id] as kennisgroep %}
{% with kennisgroep.o.hascollabmanager as managers %}
<div class="page-aside__organisation">
	<h3 class="bordered-title">Organisatie</h3>
	<h4>Beheerder</h4>
    {% include "list/list-item-person-small.tpl" id=creator context_rsc=id class="person-author" %}

	<h4>{_ Community Librarian _}</h4>
	{% for manager in managers %}
        {% include "list/list-item-person-small.tpl" id=manager context_rsc=id class="person-author" %}
	{% endfor %}
</div>
{% endwith %}
{% endwith %}
{% endwith %}
