{% extends "base.tpl" %}

{% block content %}

    {% if m.acl.user.id.o.subject %}
        {% wire action={redirect dispatch="home"} %}
    {% endif %}

    <div class="c-signup-container">
    
        <h1>{{ m.rsc.signup_step3.title }}</h1>
        <p>{{ m.rsc.signup_step3.body }}</p>

        {% wire id=#form type="submit" postback={save_signup_step3} delegate=`kenniscloud` %}
        
        <form id="{{ #form }}" method="POST" action="postback">
            <div class="form-group with-elm" id="signup_tags_group">
                <label for="signup_tags" class="control-label">In welke thema's ben je geïnteresseerd?</label>
                <input id="signup_tags_validated" hidden=true value="1">
                <div id="signup_tags_control"></div>

                {# Note: this validator is defined in "_js_include.tpl" #}
                {% validate id="signup_tags_validated" type={custom failureMessage="Vul minimaal 1 interesse in" against="window.validateSignupTags"} %}
            </div>

            <div class="u-d-flex u-flex-gap-1">
                {% button id="back" class="btn btn-default u-margin-right-auto" text=_"Back" title=_"Return to the previous page" action={redirect dispatch="signup_step2"} %}
                {% button id="skip" class="btn btn-default" text=_"Skip" title=_"Skip this step." action={redirect dispatch="home"} %}
                {% button type="submit" id="save_stay" class="btn btn-primary" text=_"Finish" title=_"Finish." %}
            </div>
        </form>
    </div>

{% endblock %}
