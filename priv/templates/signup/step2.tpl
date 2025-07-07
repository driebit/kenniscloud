{% extends "base.tpl" %}

{% block content %}

    <div id="{{ #signup }}" class="c-signup-container fade-in">

        <h1>{{ m.rsc.signup_step2.title }}</h1>
        <p>{{ m.rsc.signup_step2.body }}</p>

        {% wire id=#form type="submit" postback={save_signup_step2 } delegate=`kenniscloud` %}

        <form id="{{ #form }}" method="POST" action="postback">
            <div id="signup_region_group" class="form-group">
                <label for="region" class="control-label">Aanmelden voor regio</label>
                <div>
                    <select id="signup_region" name="signup_region" class="form-control">
                        <option value="">{_ Select _}</option>
                        {% for region in m.search[{query cat_exact=`region` sort="rsc.pivot_title"}] %}
                            <option value="{{ region.name }}">{{ region.title }}</option>
                        {% endfor %}
                    </select>
                </div>
                {% validate id="signup_region"
                    type={presence failure_message="Selecteer een regio of kies de optie \"Geen regio\""}
                %}
            </div>

            <div class="u-d-flex u-flex-gap-1">
                {% button id="back" class="btn btn-default u-d-hidden" text=_"Back" title=_"Return to the previous page" action={redirect dispatch="signup_step1"} %}
                <button type="button" id="back-step-btn" class="btn btn-default u-margin-right-auto" title=_"Return to the previous page">{_ Back _}</button>

                {% button id="skip" class="btn btn-default u-d-hidden" text=_"Skip" title=_"Skip this step." action={redirect dispatch="signup_step3"} %}
                <button type="button" id="skip-step-btn" class="btn btn-default" title=_"Skip">{_ Skip _}</button>

                {% button type="submit" id="save_stay" class="btn btn-primary u-d-hidden" text=_"Save" title=_"Save." %}
                <button type="button" id="next-step-btn" class="btn btn-primary" title=_"Save and next">{_ Save _}</button>

            </div>
        </form>
    </div>

    {% include "signup/animation_handler.tpl" signUpForm=#signup  %}
    
{% endblock %}