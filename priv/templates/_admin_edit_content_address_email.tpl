{# Copy of the same template from 'mod_admin' with the exception of the explicit
# 'delegate' set to 'validator_base_email'.
# This prevents the validator lookup from picking a different validator to render,
# particularly 'validator_crowdparticipant_email'.
#}
<div class="form-group">
	<label class="control-label" for="email">{_ E-mail address _}</label>
	<div>
		<input class="form-control" id="email" type="text" name="email" value="{{ id.email }}" />
		{% validate id="email" type={email delegate="validator_base_email"} %}
	</div>
</div>
