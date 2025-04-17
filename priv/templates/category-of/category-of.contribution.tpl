{% with
    class|default:"category-of"
as
    class
%}

<div class="category-of">
	<span class="category-of__cat">{{ m.rsc[id.category.id].title }}</span>
	<a href="{{ m.rsc[id.content_group_id.id].page_url }}" class="category-of__kg">{{ m.rsc[id.content_group_id.id].title }}</a>
    <span>{{ id.publication_start|date:"j b Y" }}</span>
    {% if id.address_city %}
    	<i class="icon--location"></i> {{ id.address_city|truncate:35 }}
    {% elseif id.haslocation[1] as loc_id %}
        <a href="{{ loc_id.page_url }}"><i class="icon--location"></i> {{ loc_id.title }}</a>
	{% endif %}
</div>

{% endwith %}
