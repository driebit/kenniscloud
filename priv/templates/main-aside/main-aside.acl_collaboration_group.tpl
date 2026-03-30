{% if m.search[{query hassubject=[id,'has_subgroup'] pagelen=100}] as kc_groups %}
    <h3 class="bordered-title">Kennisgroepen in deze kennisgroep</h3>
    {% include "list/list.tpl" list_id="list--kc_groups" items=kc_groups %}
{% endif %}

{% with m.search.paged[{query upcoming is_published content_group=id sort="rsc.pivot_date_start" cat="event" pagelen="2" }] as upcoming_events %}

	{% if upcoming_events %}
		<h3 class="bordered-title">Meetups</h3>
		{% include "list/list.tpl" list_id="list--upcoming" class="list--kg" list_template="list/list-item-kg.tpl" items=upcoming_events extraClasses="" id=id noresults=" " %}
	{% endif %}


	{% if q.tag|if:m.search.paged[{query is_published content_group=id sort=["-is_featured", "-rsc.publication_start", "id"] cat=['contribution', 'event'] hasobject=[q.tag,'subject'] id_exclude=upcoming_events|make_list pagelen=6 page=q.page}]:m.search.paged[{query is_published content_group=id sort=["-is_featured", "-rsc.publication_start", "id"] cat=['contribution', 'event'] id_exclude=upcoming_events|make_list pagelen=6 page=q.page}] as result %}

		<h3 id="contributions" class="bordered-title">Bijdragen</h3>
		{% if m.search[{facets is_published content_group=id cat=['contribution', 'event'] id_exclude=upcoming_events|make_list}] as facetted %}
			<p>
				<ul class="keywords">
					{% for tag in facetted.facets.tag.counts|slice:5 %}
						{% if tag.value == q.tag %}
							<li class="btn--secondary">
								<a href="?#contributions" title="{{ tag.label }}">{{ tag.label }} ({{tag.count}})</a>
							</li>
						{% else %}
							<li class="btn--secondary disabled">
								<a href="?tag={{ tag.value }}#contributions" title="{{ tag.label }}">{{ tag.label }} ({{tag.count}})</a>
							</li>
						{% endif %}
					{% endfor %}
				</ul>
			</p>
		{% endif %}

		{% include "list/list.tpl" list_id="list--content" class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" id=id noresults=" " exclude=upcoming_events|make_list %}

	{% endif %}

{% endwith %}


{% with m.search[{query cat="person" hassubject=[id, "hascollabmanager"] sort="-pivot.kenniscloud_users.has_depiction"}] as managers %}
{% with m.search[{query cat="person" hassubject=[id, "hascollabmember"] sort="-pivot.kenniscloud_users.has_depiction"}] as members %}
{% if (managers ++ members)|uniq as all_participants %}
    <h3 class="bordered-title project-members">Deze mensen zijn betrokken bij deze kennisgroep</h3>

    <div class="home-members__list">
	    {% for participant in all_participants %}
	        {% include "list/list-item-person-small.tpl" id=participant project=id summary  %}
	    {% endfor %}
	</div>
{% endif %}
{% endwith %}
{% endwith %}
