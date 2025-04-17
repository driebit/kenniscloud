{% extends "admin_edit_widget_std.tpl" %}

{# Show the edit fields to edit the name of a person #}

{% block widget_title %}Contactgegevens{% endblock %}
{% block widget_id %}content-address{% endblock %}
{% block widget_class %} edit-address {% endblock %}
{% block widget_show_minimized %}false{% endblock %}

{% block widget_content %}
<div class="row">
	<div class="col-lg-6 col-md-6">
		<div class="form-group address_facebook">
			<label class="control-label" for="facebook">{_ Facebook _}</label>
			<input class="form-control" id="facebook" type="text" name="facebook" value="{{ id.facebook }}">
		</div>
		<div class="form-group address_website">
			<label class="control-label" for="website">Website</label>
			<input class="form-control" id="website" name="website" value="{{ id.website }}">
		</div>
	</div>

	<div class="col-lg-6 col-md-6">
		<div class="form-group address_twitter">
			<label class="control-label" for="twitter">{_ Twitter _}</label>
			<input class="form-control" id="twitter" type="text" name="twitter" value="{{ id.twitter }}">
		</div>
		<div class="form-group address_linkedin">
			<label class="control-label" for="linkedin">{_ LinkedIn _}</label>
			<input class="form-control" id="linkedin" type="text" name="linkedin" value="{{ id.linkedin }}">
		</div>
	</div>
	<div class="col-lg-6 col-md-6">
		<div>
			<label class="control-label" for="address_city">Woonplaats</label>
			<input class="form-control" id="address_city" type="text" name="address_city" value="{{ id.address_city }}">
		</div>
	</div>
</div>
<div class="row ginger-edit-email">
	<div class="col-lg-12 col-md-12">
		{% catinclude "_admin_edit_content_address_email.tpl" id.id %}
	</div>
</div>
{% endblock %}
