{% extends "admin_edit_widget_std.tpl" %}

{# Widget for adding page actions #}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-crowdlink{% endblock %}

{% block widget_title %}
    {_ Crowd Links _}
    <div class="widget-header-tools">
        <a href="#" class="z-btn-help do_dialog" data-dialog="{{ %{ title: _"Help about Crowd Links", text: _"Create a link to the offline meetup crowd" }|escape }}" title="{_ Need more help? _}"></a>
    </div>
{% endblock %}

{% block widget_content %}
    <div id="unlink-undo-message"></div>

    <div class="form-group">
        {% button text="Create/reset link" class="btn btn-default" postback={set_crowdlink id=id} delegate="mod_crowdlink" %}
        {% button text="Delete link" class="btn btn-default" postback={delete_crowdlink id=id} delegate="mod_crowdlink" %}
        {% if id.category.name == `daycrowdevent` %}
            {% button
                text=_"Reset daycrowd"
                class="btn btn-default"
                action={confirm
                       text=_"Are you sure you want to reset this daycrowd and remove all attendances (rsvp)?"
                       action={dialog_close}
                       delegate=`mod_crowdlink`
                       postback={reset_daycrowd id=id}
                    }
            %}
        {% endif %}
    </div>
    <div class="form-group">
        {% with m.crowdlink.for_meetup[id].url as meetup_url %}
        {% with m.crowdlink.for_meetup[id].code as meetup_code %}
        {% with m.crowdlink.for_meetup[id].expiry as meetup_expiry %}
            <p>
                Current link: <a id="crowdlinks_link" href="{{meetup_url}}">{{ meetup_code }}</a> expires {{ id.is_a[`daycrowdevent`]|if:id.date_end:meetup_expiry }}
            </p>
            {% if id.category.name == `daycrowdevent` %}
                <p>
                    <a href="/crowd/{{id}}/present?crowdlink={{meetup_code}}">Daycrowd presentation link</a> (for big screen on location)
                </p>
            {% endif %}
        {% endwith %}
        {% endwith %}
        {% endwith %}
    </div>
{% endblock %}
