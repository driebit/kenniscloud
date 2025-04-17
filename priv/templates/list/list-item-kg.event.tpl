
{% if not (exclude and id.id|member:exclude) %}
	{% with id.o.hasbanner[1]|default:id.depiction.id|default:m.rsc.fallback.id as dep %}
	<li class="list-item-kg--event" style="background-image: url('{% image_url dep mediaclass='list-image' crop=dep.crop_center %}');">
		<a href="{{ id.page_url }}">
			<div class="list-item-kg-event__content">
				{% include "category-of/category-of.tpl" nolink="true" %}

				<div class="list-item-kg-event__content__text">
					{% if id.date_start|date:"Y" %}

						<time datetime="{{ id.date_start|date:"Y-F-jTH:i" }}">
							<i class="icon--calendar"></i>
							{% include "meta/date.tpl" id=id %}
						</time>
					{% endif %}
					<h3>{{ id.title|truncate:100 }}</h3>
				</div>
			</div>
		</a>
	</li>
	{% endwith %}
{% endif %}
