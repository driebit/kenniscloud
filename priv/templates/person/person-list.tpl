{% block person_list_above %}{% endblock %}

{% if m.search[{query hassubject=[id,predicate] sort="-edge.created"}] as total %}

	{% with total|slice:show as result %}
	{% with total|length-show as remainder %}

		    <h3 class="bordered-title">{{ title }}</h3>
		    <div class="person-list__items" id="person-list__items-{{ #identifier }}">
                {% include "person/person-list-loop.tpl" result=resul context_rsc=id %}
		        {% if remainder > 0 %}
					{% button class="person--total" text="<span>+ " ++ remainder|make_list ++ "</span>" action={update target="person-list__items-"++#identifier result=total context_rsc=id template="person/person-list-loop.tpl"} %}
		        {% endif %}
		    </div>

		</div>

	{% endwith %}
	{% endwith %}

{% endif %}

{% block person_list_below %}{% endblock %}
