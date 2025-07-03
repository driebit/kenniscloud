{% extends "base.tpl" %}

{% block content %}

    {% if m.acl.user.id.name_first %}
        {% wire action={redirect dispatch="home"} %}
    {% endif %}

    <div class="c-signup-container">

        <h1>{{ m.rsc.signup_step1.title }}</h1>
        <p>{{ m.rsc.signup_step1.body }}</p>

        {% wire id=#form type="submit" postback={save_signup_step1} delegate=`kenniscloud` %}
        
        <form id="{{ #form }}" method="POST" action="postback">
            {% optional include "_translation_init_languages.tpl" %}
            <div class="widget widget--name">
                <div class="widget-content">
                    <div class="form-group row u-d-flex u-flex-col">
                        <div class="form-group col-md-12">
                            <label class="control-label" for="name_first">{_ First _}</label>
                            <div>
                                <input class="form-control" id="name_first" type="text" name="name_first" value="{{ m.acl.user.id.name_first }}" />
                            </div>
                        </div>
                        <div class="form-group col-md-12">
                            <label class="control-label" for="name_surname_prefix">{_ Sur. prefix _}</label>
                            <div>
                                <input class="form-control" id="name_surname_prefix" type="text" name="name_surname_prefix" value="{{ m.acl.user.id.name_surname_prefix }}" />
                            </div>
                        </div>
                        <div class="form-group col-md-12">
                            <label class="control-label" for="name_surname">{_ Surname _}</label>
                            <div>
                                <input class="form-control" id="name_surname" type="text" name="name_surname" value="{{ m.acl.user.id.name_surname }}" />
                            </div>
                        </div>
                    </div>
                </div>
                <div class="u-d-flex u-flex-gap-1">
                    {% button type="submit" id="save_stay" class="btn btn-primary u-margin-left-auto" text=_"Save" title=_"Save and next" %}
                </div>
            </div>
        </form>
    </div>

{% endblock %}