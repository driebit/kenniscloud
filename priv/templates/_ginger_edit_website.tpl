{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Bron _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}content-address{% endblock %}
{% block widget_class %} edit-address {% endblock %}

{% block widget_content %}

	<div class="row">
		<div class="col-lg-12 col-md-12">
			<div class="form-group address_website">
				<p class="helper-text">Plaats hier de link naar je bron.</p>
				<input class="form-control" id="website" name="website" value="{{ id.website }}">
			</div>
		</div>
	</div>

{% endblock %}
