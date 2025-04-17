{% include "page-actions/page-action-rsvp.tpl" btn_class="btn--primary" btn_connect_text='Reserveer' btn_cancel_text='Annuleer reservering' %}

{% if m.search[{query hasobject=[id,"rsvp"] sort="-edge.created"}] as total %}

	{% with total|slice:show as result %}
	{% with total|length-show as remainder %}

		    <h3 class="bordered-title">{{ title }}</h3>
		    <div class="person-list__items" id="person-list__items-{{ #identifier }}">
                {% include "person/person-list-loop.tpl" result=result context_rsc=id %}
		        {% if remainder > 0 %}
					{% button class="person--total" text="<span>+ " ++ remainder|make_list ++ "</span>" action={update target="person-list__items-"++#identifier result=total context_rsc=id template="person/person-list-loop.tpl"} %}
		        {% endif %}
		    </div>

	{% endwith %}
	{% endwith %}

{% endif %}
