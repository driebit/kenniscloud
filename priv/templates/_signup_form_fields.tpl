{% include "_signup_form_fields_email.tpl" %}
{% include "_signup_form_fields_username.tpl"
    show_signup_username=false
    show_signup_username_title=false
%}
{% include "_signup_form_fields_region.tpl" %}
{% include "_signup_form_fields_keywords.tpl" %}
{% include "_signup_form_fields_tos.tpl"
    show_signup_tos_title=false
    show_signup_tos_info=false
%}
<div class="form-group" id="signup_button">
    <button type="submit" class="btn btn-primary">{_ Sign Up _}</button>
</div>
