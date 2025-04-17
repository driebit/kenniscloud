{% extends "admin_edit_widget_std.tpl" %}

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
            <label class="control-label" for="address_street_1">{_ Street _}</label>
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
{% endblock %}
