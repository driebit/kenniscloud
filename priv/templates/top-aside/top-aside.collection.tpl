<div class="page-aside__text">
	<p></p>

	{% if not m.acl.user %}
		<a href="{% url logon p={page id=id}|url %}" class="btn--primary">Aanmelden</a>
	{% endif %}
</div>
