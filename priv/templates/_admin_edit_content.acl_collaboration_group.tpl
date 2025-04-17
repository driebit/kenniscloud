{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
{_ Additional copy _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_i18n_tab_class %}item{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-notifications{% endblock %}

{% block widget_content %}
{% with not id or m.rsc[id].is_editable as is_editable %}
<fieldset class="form-horizontal">
    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #email_welcome_message }}{{ lang_code_for_id }}">{_ Welcome to kennisgroep email message _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #email_welcome_message }}{{ lang_code_for_id }}" name="email_welcome_message{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : id.translation[lang_code].email_welcome_message : id.email_welcome_message | brlinebreaks }}</textarea>
        </div>
    </div>
</fieldset>
{% endwith %}
{% endblock %}
