<div class="{% block cta_class %}cta{% endblock %}">
		<h2>
				{% block cta_title %}
						Help de kennis groeien, doe mee!
				{% endblock %}
		</h2>

		<a href="{{ m.rsc.page_kennisgroepen.page_url }}" class="btn--primary">Bekijk alle kennisgroepen</a>

		{% if not m.acl.user %}
				<a href="{% url logon p={page id=id}|url %}" class="btn--primary">Doe mee!</a>
		{% endif %}
</div>
