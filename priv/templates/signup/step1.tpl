{% extends "base.tpl" %}

{% block content %}
<div class="container">
    <h1>{_ Sign Up Page 1/3 _}</h1>

</div>
    {% wire id=#form type="submit" postback={save_signup_step1} delegate=`kenniscloud` %}
    <form id="{{ #form }}" method="POST" action="postback">
    <div id="poststuff">
    {% optional include "_translation_init_languages.tpl" %}
    <div class="widget widget--name">
        <div class="widget-content">
            <div class="form-group row">
                <div class="form-group col-lg-4 col-md-4">
                    <label class="control-label" for="name_first">{_ First _}</label>
                    <div>
                        <input class="form-control" id="name_first" type="text" name="name_first" value="{{ m.acl.user.id.name_first }}" />
                    </div>
                </div>
                <div class="form-group col-lg-2 col-md-2">
                    <label class="control-label" for="name_middle">{_ Middle _}</label>
                    <div>
                        <input class="form-control" id="name_middle" type="text" name="name_middle" value="{{ m.acl.user.id.name_middle }}" />
                    </div>
                </div>
                <div class="form-group col-lg-2 col-md-2">
                    <label class="control-label" for="name_surname_prefix">{_ Sur. prefix _}</label>
                    <div>
                        <input class="form-control" id="name_surname_prefix" type="text" name="name_surname_prefix" value="{{ m.acl.user.id.name_surname_prefix }}" />
                    </div>
                </div>
                <div class="form-group col-lg-4 col-md-4">
                    <label class="control-label" for="name_surname">{_ Surname _}</label>
                    <div>
                        <input class="form-control" id="name_surname" type="text" name="name_surname" value="{{ m.acl.user.id.name_surname }}" />
                    </div>
                </div>
            </div>
        </div>
        {% button type="submit" id="save_stay" class="btn btn-default" text=_"Save" title=_"Save this page." %}
    </div>
    </form>
{% endblock %}