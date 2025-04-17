{% with
class|default:"category-of",
(id.content_group_id.name == "default_content_group")|if:"KennisCloud":id.content_group_id.title
as
class,
content_group_title
%}

<div class="category-of">
	<span class="category-of__cat">{{ m.rsc[id.category.id].title }}</span>
	<a href="{{ id.content_group_id.page_url }}" class="category-of__kg">{{ content_group_title }}</a>
	{% if id.address_city %}
    	<i class="icon--location"></i> {{ id.address_city|truncate:35 }}
    {% elseif id.haslocation[1] as loc_id %}
        <a href="{{ loc_id.page_url }}"><i class="icon--location"></i> {{ loc_id.title }}</a>
	{% endif %}
</div>

{% endwith %}
