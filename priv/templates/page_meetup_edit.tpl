{% extends "ginger_edit_base.tpl" %}

{% block title %}{% if id %}Meetup bewerken{% else %}Meetup toevoegen{% endif %}{% endblock%}

{% block page_class %}ginger-edit{% endblock %}

{% block container %}
    {% with "event" as category %}
    {% with id|temporary_rsc:{props category=category} as id %}
    {% inherit %}
    {% endwith %}
    {% endwith %}
{% endblock %}

{% block header %}

    {% if m.acl.user %}
    <header class="ginger-edit__header">
        <nav class="ginger-edit__nav">
            {% button class="btn--save" text=_"Plaatsen" title=_"Plaatsen"
                      action={script script="$('#save_view').click();"}
             %}
             {% button class="btn--cancel" text=_"Cancel" action={redirect back} %}

        </nav>

        {% block header_text %}
            <h1>Meetup toevoegen</h1>
        {% endblock %}
    {% endif %}
    </header>
{% endblock %}

{% block content %}
    {% if m.acl.user %}
        <form id="rscform" method="post" action="postback" class="form">
        <div class="row page-ginger_edit_content_row_class">
            <div class="col-sm-8 col-md-8">
                {% include "_ginger_edit.event.tpl" isnew=`true` %}
            </div>

            <div class="col-sm-4 col-md-4">
                {% include "_aside_ginger_edit.event.tpl" author=q.auteur %}
            </div>
        </div>
        </form>
    {% else %}
        <h2>Om een verhaal toe te voegen heb je een account nodig.</h2>
         <a href="{% url logon p=m.req.raw_path %}" class="btn--primary">{_ Logon _}</a>
         <a href="{% url signup p=m.req.raw_path %}" class="btn--primary">{_ Register _}</a>
    {% endif %}

{% endblock %}

{% block footer %}

    {% if m.acl.user %}
    <nav class="ginger-edit__nav--footer">
        <div class="navbar-inner row">
            <div class="col-xs-12" id="save-buttons">
                {% button class="btn--save" text=_"Plaatsen" title=_"Plaatsen"
                        action={script script="$('#save_view').click();"}
                 %}

                {% button class="btn--cancel" text=_"Cancel" action={redirect back}%}
            </div>
        </div>
    </nav>
    {% endif %}
    {% include "_ginger-editor.tpl" overrides_tpl="_ginger_edit_tinymce_overrides_js.tpl" %}
{% endblock %}
