{% extends "_admin_edit_basics.tpl" %}

{% block widget_content_nolang %}
<div class="form-group row">
    <label class="control-label col-md-12" for="is_private">
        <input type="checkbox" id="is_private" name="is_private"
            value="1"
            {% if m.kc_collab_group[id].private_acl_rule_id|is_defined %}checked{% endif %}
            {% if not id.is_editable %}disabled="disabled"{% endif %} />
        {_ Verborgen kennisgroep _}
    </label>
</div>
<div class="form-group row">
    <label class="control-label col-md-12" for="has_map">
        <input type="checkbox" id="has_map" name="has_map"
            value="1"
            {% if id.has_map %}checked{% endif %}
            {% if not id.is_editable %}disabled="disabled"{% endif %} />
        {_ Kaart opnemen _}
    </label>
</div>
<div class="form-group row">
    <label class="control-label col-md-12" for="timeline_off">
        <input type="radio" id="timeline_off" name="timeline_status"
            value="off"
            {% if id.timeline_status == "off" or not id.timeline_status %}checked{% endif %}/>
        {_ Timeline off _}
    </label>
    <label class="control-label col-md-12" for="timeline_automatic">
        <input type="radio" id="timeline_automatic" name="timeline_status"
            value="automatic"
            {% if id.timeline_status == "automatic" %}checked{% endif %}/>
        {_ Automatic timeline _}
    </label>
    <label class="control-label col-md-12" for="activate_timeline">
        <input type="radio" id="activate_timeline" name="timeline_status"
            value="manual"
            {% if id.timeline_status == "manual" %}checked{% endif %}/>
        {_ Manual timeline _}
    </label>
</div>
{% endblock %}
