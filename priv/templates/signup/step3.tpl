{% extends "base.tpl" %}

{% block content %}

    {% if m.acl.user.id.o.subject %}
        {% wire action={redirect dispatch="page" id=m.acl.user.id } %}
    {% endif %}

    <div id="{{ #signup }}" class="c-signup-container fade-in">
    
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
                {% button id="back" class="btn btn-default u-d-hidden" text=_"Back" title=_"Return to the previous page" action={redirect dispatch="signup_step2"} %}
                <button type="button" id="back-step-btn" class="btn btn-default u-margin-right-auto" title=_"Return to the previous page">{_ Back _}</button>

                {% button id="skip" class="btn btn-default u-d-hidden" text=_"Skip" title=_"Skip this step." action={redirect dispatch="home"} %}
                <button type="button" id="skip-step-btn" class="btn btn-default" title=_"Skip">{_ Skip _}</button>

                {% button type="submit" id="save_stay" class="btn btn-primary u-d-hidden" text=_"Save" title=_"Save." %}
                <button type="button" id="next-step-btn" class="btn btn-primary" title=_"Save and next">{_ Save _}</button>
            </div>
        </form>
    </div>

    {% include "signup/animation_handler.tpl" signUpForm=#signup  %}

{% endblock %}
