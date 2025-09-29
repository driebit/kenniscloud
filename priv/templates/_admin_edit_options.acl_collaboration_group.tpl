{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
{_ Options _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_i18n_tab_class %}item{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-options{% endblock %}

{% block widget_content_nolang %}
<h3>{_ Options _}</h3>
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
<div class="form-group row">
    <h6 class="col-md-12">{_ Sorting order _}</h5>
    <label class="control-label col-md-12" for="timeline_asc">
        <input type="radio" id="timeline_asc" name="timeline_order"
            value="asc"
            {% if id.timeline_order == "asc" or not id.timeline_order %}checked{% endif %}/>
        {_ Ascending _}
    </label>
    <label class="control-label col-md-12" for="timeline_desc">
        <input type="radio" id="timeline_desc" name="timeline_order"
            value="desc"
            {% if id.timeline_order == "desc" %}checked{% endif %}/>
        {_ Descending _}
    </label>
</div>
{% if m.acl.use.mod_admin %}
    <h4 style="margin-top: 40px;">{_ Poll embedden via Mentimeter of OpnForm _}</h4>
    <div class="form-group row col-md-12">
        <input type="text" id="has_mentilink" name="has_mentilink" value="{{ id.has_mentilink }}" placeholder="abcvshfkdl" class="ltr do_autofocus field-title form-control"
                {% if not id.is_editable %}disabled="disabled"{% endif %} />
            {_ Mentimeter embed id _}
        <label class="control-label" for="has_mentilink">
        {_ to include a presentation you need to use the embed code in the following format _}: "app/presentation/al3bdpfthdpo9h5dwqjoch24ej6ofskc"
        </label>
    </div>

    <div class="form-group row col-md-12">
        <input type="text" id="has_opnformlink" name="has_opnformlink" value="{{ id.has_opnformlink }}" placeholder="customer-feedback-survey-9l6tuv" class="ltr do_autofocus field-title form-control"
                {% if not id.is_editable %}disabled="disabled"{% endif %} />
            {_ OpnForm embed id _}
        <label class="control-label" for="has_opnformlink">
        </label>
    </div>
{% endif %}
{% endblock %}