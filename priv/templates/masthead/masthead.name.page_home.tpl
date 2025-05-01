{% with id.o.hasbanner[1].depiction.id  as dep %}
	<div class="masthead--home {{ extraClasses }}" style="background-image: url({% image_url dep mediaclass='masthead' crop=dep.crop_center %}); background-size: cover;">
		<div class="main-container">
			<div class="masthead__content">
				{% include "page-title/page-title.tpl" %}
				{% if not m.acl.user %}
					{% include "summary/summary.tpl" %}
				{% endif %}
			</div>
			{% if not m.acl.user %}
				<a href="{% url logon p={page id=id}|url %}" class="btn--primary">Doe mee!</a>
			{% else %}
				<a href="{{ m.rsc.page_kennisgroepen.page_url }}" class="btn--primary -icon">Bekijk de kennisgroepen</a>
				{# {% include "page-actions/page-action-create-group.tpl" id=id tabs_enable=["new"] btn_class="btn--primary" btn_title="Start nu je eigen kennisgroep" %} #}
			{% endif %}
		</div>

	</div>
{% endwith %}
