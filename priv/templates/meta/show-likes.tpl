{% if id.s.like as likes %}

	<div class="total-likes">
		<i class="icon--like-small"></i> {{ likes|length }} waardering{% if likes|length > 1 %}en{% endif %}

{% if nolink == "false" %}
			<div class="total-likes__inner">
				{% for like in likes %}
                    {% include "list/list-item-person-small.tpl" class="person-author" id=like context_rsc=id %}
				{% endfor %}
			</div>
		{% endif %}
	</div>

{% endif %}
