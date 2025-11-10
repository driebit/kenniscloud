{% with slice_items|default:200 as slice_items %}
	{% if id.o.subject as results %}
	    <ul class="keywords">
	        {% for id in results|slice:slice_items %}
	            {% if id.is_visible %}
	                <li class="btn--secondary">
	                	{% if nolink == "true" %}
							<div title="{{ m.rsc[id].title }}">{{ m.rsc[id].title|truncate:12 }}</div>
	               		{% else %}
	                		<a href="/search?tag={{ id }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a>
	                	{% endif %}
	                </li>
	            {% endif %}
	        {% endfor %}
	    </ul>
	{% endif %}
{% endwith %}
