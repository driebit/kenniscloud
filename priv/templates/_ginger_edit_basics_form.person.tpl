{% with not id or id.is_editable as is_editable %}
    <div class="row">
        <div class="form-group col-xs-12">
            <label class="control-label" for="{{ #function }}{{ lang_code_for_id }}">Functie (optioneel)</label>
            <input type="text" id="{{ #function }}{{ lang_code_for_id }}" name="function{{ lang_code_with_dollar }}" value="{{ is_i18n|if : id.translation[lang_code].function : id.function }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class=" field-function form-control" %}>
        </div>
    </div>

    <div class="row">
        <div class="form-group col-xs-12">
            <label class="control-label" for="{{ #summary }}{{ lang_code_for_id }}">Korte introductie (optioneel)</label>
            <small class="helper-text">Hou dit kort en bondig. Dit stukje wordt getoond onder het kopje "Over mijzelf" op je profielpagina.</small>
            <textarea rows="4" cols="10" id="{{ #summary }}{{ lang_code_for_id }}" name="summary{{ lang_code_with_dollar }}" {% include "_language_attrs.tpl" language=lang_code class=" intro form-control" %}>{{ id.summary | brlinebreaks }}</textarea>
        </div>
    </div>
{% endwith %}
