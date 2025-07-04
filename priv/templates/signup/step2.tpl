{% extends "base.tpl" %}

{% block content %}

    {% if m.acl.user.id.o.hasregion %}
        {% wire action={redirect dispatch="page" id=m.acl.user.id } %}
    {% endif %}

    <div class="c-signup-container">

        <h1>{{ m.rsc.signup_step2.title }}</h1>
        <p>{{ m.rsc.signup_step2.body }}</p>

        {% wire id=#form type="submit" postback={save_signup_step2 } delegate=`kenniscloud` %}

        <form id="{{ #form }}" method="POST" action="postback">
            <div id="signup_region_group" class="form-group">
                <label for="region" class="control-label">Aanmelden voor regio</label>
                <div>
                    <select id="signup_region" name="signup_region" class="form-control">
                        <option value="">{_ Select _}</option>
                        {% with m.rsc[`region_none`] as region_none %}
                            {% with m.search[{query cat_exact=`region` id_exclude=region_none.id sort="rsc.pivot_title"}] as regions %}
                                {% for region in regions %}
                                    <option value="{{ region.name }}" {% if m.acl.user.id.o.hasregion == region.id %}selected{% endif %}>{{ region.title }}</option>
                                {% endfor %}
                            {% endwith %}
                            <option value="{{ region_none.name }}">{{ region_none.title }}</option>
                        {% endwith %}
                    </select>
                </div>
                {% validate id="signup_region"
                    type={presence failure_message="Selecteer een regio of kies de optie \"Geen regio\""}
                %}
            </div>

            <div class="u-d-flex u-flex-gap-1">
                {% button id="back" class="btn btn-default u-margin-right-auto" text=_"Back" title=_"Return to the previous page" action={redirect dispatch="signup_step1"} %}
                {% button id="skip" class="btn btn-default" text=_"Skip" title=_"Skip this step." action={redirect dispatch="signup_step3"} %}
                {% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save." %}
            </div>
        </form>
    </div>
    
{% endblock %}