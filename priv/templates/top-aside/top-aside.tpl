{% with id.o.author|default:[id.creator_id] as creators %}
<div class="page-aside__author">
	<h3 class="bordered-title">Gestart door</h3>
	{% for r in creators %}
        {% include "list/list-item-person-small.tpl" id=r context_rsc=id class="person-author" %}
	{% endfor %}
</div>
{% endwith %}

<div class="page-aside__involvement person-list__items">
	{% if id.id|kc_get_remarkers:"about" as remarks %}
		<h3 class="bordered-title">Deze mensen praten mee</h3>
		{% for r in remarks %}
            {% include "list/list-item-person-small.tpl" id=r.creator_id context_rsc=id %}
		{% endfor %}
	{% endif %}
</div>

<div class="page-aside__share">
    <h3 class="bordered-title">Delen</h3>
    {% include "share/share.tpl" %}
</div>

{% live
    template="top-aside/_references.tpl"
    topic={object id=id predicate=`hasreference`}
    id=id
%}
