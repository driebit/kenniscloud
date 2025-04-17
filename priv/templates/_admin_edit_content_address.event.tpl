{% extends "admin_edit_widget_std.tpl" %}

{# !!!!!!!! Can be removed after ginger 0.16 is online !!!!!!!!! #}
{# Show the edit fields to edit the name of a person #}

{% block widget_title %}{_ Address _}{% endblock %}
{% block widget_id %}content-address{% endblock %}
{% block widget_class %} edit-address {% endblock %}
{% block widget_show_minimized %}false{% endblock %}

{% block widget_content %}
<div class="row">
    <div class="form-group visit_address_country col-xs-12">
        <label class="control-label" for="address_country">{_ Country _}</label>
        <span class="admin-text-header"></span>
        {% if m.modules.active.mod_l10n %}
            <select class="form-control" id="address_country" name="address_country">
                <option value=""></option>
                {% optional include "_l10n_country_options.tpl" country=id.address_country|default:"nl" %}
            </select>
        {% else %}
            <input class="form-control" id="address_country" type="text" name="address_country" value="{{ id.address_country }}">
        {% endif %}
    </div>

    <div id="visit_address" class="visit-address">
        <div class="form-group address_street col-lg-6 col-md-6">
            <label class="control-label" for="address_street_1">{_ Street_}</label>
            <input class="form-control" id="address_street_1" type="text" name="address_street_1" value="{{ id.address_street_1 }}">
        </div>

        <div class="form-group address_city col-lg-6 col-md-6">
            <label class="control-label" for="address_city">{_ City _}</label>
            <input class="form-control" id="address_city" type="text" name="address_city" value="{{ id.address_city }}">
        </div>

        <div class="form-group address_zipcode col-lg-6 col-md-6">
            <label class="control-label" for="address_postcode">{_ Postcode _}</label>
            <input class="form-control" id="address_postcode" type="text" name="address_postcode" value="{{ id.address_postcode }}">
        </div>
        <div class="form-group address_zipcode col-lg-6 col-md-6">
            <label class="control-label" for="address_title">{_ Titel van de locatie _}</label>
            <input class="form-control" id="address_title" type="text" name="address_title" value="{{ id.address_title }}">
        </div>
    </div>
</div>

<div class="row">
    <div class="col-lg-6 col-md-6">
        <div class="form-group address_facebook">
            <label class="control-label" for="facebook">{_ Facebook _}</label>
            <input class="form-control" id="facebook" type="text" name="facebook"
                    value="{{ id.facebook }}">
        </div>
        <div class="form-group address_twitter">
            <label class="control-label" for="twitter">{_ Twitter _}</label>
            <input class="form-control" id="twitter" type="text" name="twitter"
                    value="{{ id.twitter }}">
        </div>
    </div>

    <div class="col-lg-6 col-md-6">
        <div class="form-group address_website">
            <label class="control-label" for="website">{_ Website _}</label>
            <input class="form-control" id="website" name="website" value="{{ id.website }}">
        </div>
        {% if zotonic_dispatch!="ginger_edit" %}
        <div class="form-group checkbox">
            <label>
                <input type="checkbox" id="field-is-website=redirect" name="is_website_redirect"
                        value="1"
                        {% if id.is_website_redirect %}checked{% endif %}
                        {% if not is_editable %}disabled="disabled"{% endif %}
                />
                {_ Redirect to website on page view _}
            </label>
        </div>
        {% endif %}
        <div class="form-group address_linkedin">
            <label class="control-label" for="linkedin">{_ LinkedIn _}</label>
            <input class="form-control" id="linkedin" type="text" name="linkedin"
                    value="{{ id.linkedin }}">
        </div>
    </div>
</div>

{% endblock %}
