{# display hidden if no remarks so JS can update this in the DOM #}

{% with id.id|kc_count_remarks:"about" as count %}
	<div class="total-remarks {% if count==0 %}no-remarks{% endif %}" {% if count==0 %}style="display:none;"{% endif %}>
		{% if nolink == "true" %}
			<i class="icon--remarks"></i><span class="listitem-remarks--count">{% include "meta/remark-number-of.tpl" %}</span>
		{% else %}
			<a href="#reacties" class="do_anchor" data-offset="40"><i class="icon--remarks"></i><span class="total-remarks--count">{% include "meta/remark-number-of.tpl" %}</span></a>
		{% endif %}
	</div>
{% endwith %}
