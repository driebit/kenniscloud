{% extends "admin_base.tpl" %}

{% block title %}{_ Temporarily stored crowd participant data _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Temporarily stored crowd participant data _}</h2>
    </div>
    {% if m.acl.is_allowed.use.mod_crowdparticipant %}
        {% with m.crowdparticipant.all as results %}
            <table class="table table-striped do_adminLinkedTable">
                <thead>
                    <tr>
                        <th>
                            {_ Person Id _}
                        </th>
                        <th>
                            {_ Meetup Id _}
                        </th>
                        <th>
                            {_ Name _}
                        </th>
                        <th>
                            {_ Email _}
                        </th>
                        <th>
                            {_ Contact in profile _}
                        </th>
                        <th>
                            {_ Expiry _}
                        </th>
                        <th>
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {% for result in results %}
                    <tr>
                        <td>
                            <a href="/admin/edit/{{ result[1] }}"> {{ result[1] }} </a>
                        </td>
                        <td>
                            <a href="/crowd/{{ result[2] }}"> {{ result[2] }} </a>
                        </td>
                        <td>
                            {{ result[3] }}
                        </td>
                        <td>
                            {{ result[4] }}
                        </td>
                        <td>
                            {{ result[5] }}
                        </td>
                        <td>
                            {{ result[6] }}
                        </td>
                        <td>
                            {% button text="Remove" postback={delete_crowdparticipant id=result[1]} delegate="mod_crowdparticipant" %}
                        </td>
                    </tr>
                    {% endfor %}
                </tbody>
            </table>
            {% endwith %}
    {% else %}
        <div class="alert alert-info">
            {_ Only administrators are allowed to see temporarily stored crowd participant data _}
        </div>
    {% endif %}

{% endblock %}
