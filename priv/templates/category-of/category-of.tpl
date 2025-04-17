{% with
    class|default:"category-of"
as
    class
%}

<div class="category-of">
	<span class="category-of__cat">
		{% if id.o.hasusergroup.name == "acl_user_group_community_librarian" %}
			Community Librarian
        {% elseif id|member:collab_group.o.hascollabmanager %}
			Beheerder
		{% elseif id|is_a:"reference" or id|is_a:"event" or id|is_a:"contribution" or id|is_a:"person" or id|is_a:"acl_collaboration_group" or id|is_a:"region" %}
			{{ m.rsc[id.category.id].title }}
        {% else %}
            Pagina
		{% endif %}
	</span>
	{% if rsc_id and not nolink %}
		<a href="{{ m.rsc[rsc_id].page_url }}" class="category-of__kg">{{ m.rsc[rsc_id].title }}</a>
    {% elseif rsc_id %}
        <span class="category-of__kg">{{ m.rsc[rsc_id].title }}</div>
	{% endif %}
	{% if id.address_city %}
    	<i class="icon--location"></i> {{ id.address_city|truncate:30 }}
    {% elseif id.haslocation|first as loc_id %}
        {% if nolink %}
            <i class="icon--location"></i> {{ loc_id.title|truncate:30 }}
        {% else %}
            <a href="{{ loc_id.page_url }}"><i class="icon--location"></i> {{ loc_id.title|truncate:35 }}</a>
        {% endif %}
	{% endif %}
</div>

{% endwith %}
