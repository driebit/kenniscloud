{% with not id or id.is_editable as is_editable %}
    <div class="row">
        <div class="form-group col-xs-12">
            <label class="control-label" for="{{ #title }}{{ lang_code_for_id }}">Titel bron</label>
            <input type="text" id="{{ #title }}{{ lang_code_for_id }}" name="title{{ lang_code_with_dollar }}" value="{{ is_i18n|if : id.translation[lang_code].title : id.title }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="do_autofocus field-title form-control" %}>
        </div>
    </div>

    <div class="row">
        <div class="form-group col-xs-12">
            <label class="control-label" for="summary">Korte inleiding (optioneel)</label>
            <textarea rows="4" cols="10" id="summary" name="summary{{ lang_code_with_dollar }}" class="ltr intro form-control">{{ id.summary|brlinebreaks }}</textarea>
        </div>
    </div>
{% endwith %}
