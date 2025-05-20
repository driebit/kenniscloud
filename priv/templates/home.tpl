{% extends "base.tpl" %}

{% block body_class %}t--home c-homepage{% endblock %}

{% block content %}

	{% catinclude "masthead/masthead.tpl" id %}

	{% if m.acl.user %}

		{% include "homepage-feed/user-feed.tpl" %}

		<div class="home-kg">
			<div class="main-container">
				<h2 class="bordered-title">Ook Interessant</h2>
			</div>

			{% if m.kc_user.recommended_knowledge_groups as items  %}
				{% include "list/list.tpl" items=items hide_showmore_button extraClasses="" id=id %}
			{% endif %}

			<div class="main-container">
				<a href="{{ m.rsc.page_kennisgroepen.page_url }}" class="btn--primary">
					Bekijk alle kennisgroepen
				</a>
			</div>
		</div>

	{% else %}

		<div class="home-kg">
			<div class="main-container">
				<h2 class="bordered-title">Kennisgroepen</h2>
				{% if id.o.haspart as result %}
					{% include "list/list.tpl" items=result hide_showmore_button extraClasses="" id=id %}
				{% endif %}
				<a href="{{ m.rsc.page_kennisgroepen.page_url }}" class="btn--primary">
					Bekijk alle kennisgroepen
				</a>
			</div>
		</div>

	{% endif %}

    <div class="home-regions">
        <div class="main-container">
            <h2 class="bordered-title">Deze regio's zijn al aangesloten</h2>
            {% with m.search[{query cat="region"}] as result %}
                {% for r in result %}
                    {% if r.name != "region_none" %}
                        <a href="{{ r.page_url }}" class="btn--primary -blue -icon">{{ r.title }}</a>
                    {% endif %}
                {% endfor %}
            {% endwith %}
        </div>
    </div>

	<div class="home-members">
		<div class="main-container">
			<h2 class="bordered-title">Wij maken KennisCloud</h2>
			<div class="home-members__list">
				{% with m.search[{query
                                     cat="person"
                                     pagelen=24
                                     sort="-pivot.kenniscloud_users.has_depiction"}] as result %}
					{% for r in result %}
						{% include "list/list-item-person-small.tpl" id=r summary %}
					{% endfor %}
				{% endwith %}
			</div>
			<a href="{{ m.rsc.page_community.page_url }}" class="btn--primary">Bekijk iedereen die meedoet</a>
		</div>
	</div>
{% endblock %}
