{% extends "depiction/with_depiction.tpl" %}

{% block with_depiction %}

{% if id.is_visible %}
{% with id.content_group_id as kg %}
{% with m.search.paged[{query is_published content_group=kg sort=["-is_featured", "-rsc.publication_start", "id"] cat_exclude=["remark", "keyword", "reference"] cat_exclude_defaults pagelen=1000}]|length as contributions_total %}
	<li class="list__item  {{ extraClasses }}">

		<a href="{{ id.page_url }}" class="is-kg">
			<article>
				<div class="list__item__image">
					{% image dep_rsc.id mediaclass="list-image" alt="" title="" crop=dep_rsc.crop_center %}
				</div>
				<div class="list__item__content">
					<div class="list__item__title">
						{% include "category-of/category-of.tpl" nolink="true" %}

						<h3>
                            {% if id.short_title %}
                                {{ id.short_title|truncate:50 }}
                            {% else %}
                                {{ id.title|truncate:50 }}
                            {% endif %}
                        </h3>
					</div>
					{% include "keywords/keywords.tpl" nolink="true" slice_items=5 %}

					<div class="list__item__meta">
						<p>Aantal leden <b>{{ kg|kc_collaboration_group_members|length }}</b></p>
						<p>Aantal bijdragen <b>{{ contributions_total }}</b></p>
					</div>
				</div>
			</article>
		</a>
	</li>
{% endwith %}
{% endwith %}
{% endif %}

{% endblock %}
