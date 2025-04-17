{% extends "admin_edit_widget_std.tpl" %}

{# Widget for adding page actions #}

{% block widget_show_minimized %}{{ not m.crowdparticipant[id].has_data }}{% endblock %}
{% block widget_id %}sidebar-crowdparticipant{% endblock %}

{% block widget_title %}
{_ Crowd Participant Data _}
<div class="widget-header-tools">
    <a href="#" class="z-btn-help do_dialog" data-dialog="{{ %{ title: _"Help about Crowd Participants", text: _"Manage temporarily stored personal data expiring 3 days after the corresponding Crowd meetup ends." }|escape }}" title="{_ Need more help? _}"></a>
</div>
{% endblock %}
{% block widget_content %}
        <dl>
        <dt>{{ _"Name" }}</dt>
        <dd>{{ m.crowdparticipant[id].name }}</dd>
        <dt>{{ _"Email" }}</dt>
        <dd>{{ m.crowdparticipant[id].email }}</dd>
        <dt>{{ _"Contact in profile" }}</dt>
        <dd>{{ m.crowdparticipant[id].show_contact }}</dd>
        <dt>{{ _"Expiry" }}</dt>
        <dd>{{ m.crowdparticipant[id].expires }}</dd>
        </dl>
{% endblock %}
