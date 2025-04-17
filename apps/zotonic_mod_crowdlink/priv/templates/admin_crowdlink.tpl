{% extends "admin_base.tpl" %}

{% block title %}{_ Crowdlinks _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Crowdlinks _}</h2>
    </div>
    {% if m.acl.is_allowed.use.mod_crowdlink %}
        {% with m.crowdlink.all as results %}
            <table class="table table-striped do_adminLinkedTable">
                <thead>
                    <tr>
                        <th>
                            {_ Meetup Id _}
                        </th>
                        <th>
                            {_ Url _}
                        </th>
                        <th>
                            {_ Expiry _}
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {% for result in results %}
                    <tr>
                        <td>
                            <a href="/admin/edit/{{ result[2] }}"> {{ result[2] }} </a>
                        </td>
                        <td>
                            <a href="/crowd/{{ result[2] }}?crowdlink={{ result[1] }}"> {{ result[1] }} </a>
                        </td>
                        <td>
                            {{ result[3] }}
                        </td>
                    </tr>
                    {% endfor %}
                </tbody>
            </table>
            {% endwith %}
    {% else %}
        <div class="alert alert-info">
            {_ Only administrators are allowed to see crowd links _}
        </div>
    {% endif %}

{% endblock %}
