{% extends "page.event.tpl" %}

{% block page_actions %}
	{% include "page-actions/page-action-like.tpl" %}
{% endblock %}

{% block content %}

	<main>
		<div class="page-container">

			<div class="page-intro page-intro--{{id.category.name}}">
				{% catinclude "category-of/category-of.tpl" id rsc_id=id.content_group_id %}
				{% button text=_"Back" class="btn--primary c-btn--back" action={redirect back} %}

				{% block page_actions %}{% endblock %}

				{% include "page-title/page-title.tpl" %}

                {% catinclude "media/media.tpl" id %}

				{% block page_summary %}
					{% include "summary/summary.tpl" %}
				{% endblock %}

				{% catinclude "keywords/keywords.tpl" id %}

				{% with id.media|without_embedded_media:id|first as dep %}
	                {% catinclude "media/media.tpl" dep %}
	            {% endwith %}

			</div>

			{% if id.body %}
				<div class="page-body">
					<h3 class="bordered-title">Over het programma</h3>
					{% include "body/body.tpl" %}
				</div>
			{% endif %}

			{% block content_right %}
				<aside class="page-aside">
                    {% catinclude "main-aside/main-aside.tpl" id %}
                    {% include "page-actions/page-action-edit-thing.tpl" %}
				</aside>
			{% endblock %}

		</div>

		{% block content_bottom %}
		{% endblock %}

	</main>
{% endblock %}
