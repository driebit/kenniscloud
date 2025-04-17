{% with m.acl.user as user %}
{% with id.o.hascollabmember|make_list as members %}
{% with id.o.hascollabmanager|make_list as managers %}
{% with members ++ managers as group_members %}

{% if not user|member:group_members %}

    <a id="{{ #join_group }}" href="#connect"
        class="btn--primary -blue-border -icon">
        Doe mee met deze groep
    </a>

    {% wire id=#join_group
        action={dialog_open
            template="page-actions/action-dialog-join-group.tpl"
            logon_required
            title=_"Sluit je bij ons aan"
            id=id
            user=user
            group_id=id.content_group_id
        }
    %}

{% else %}

    <a id="{{ #leave_group }}" href="#connect"
        class="btn--dark -icon">
        Deze kennisgroep verlaten
    </a>

    {% wire id=#leave_group
        action={dialog_open
            template="page-actions/action-dialog-join-group.tpl"
            leave
            title=_"Je wilt de kennisgroep verlaten"
            id=id
            user=user
            group_id=id.content_group_id
        }
    %}

{% endif %}

{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}

{% if q.qstatus=="join-group" %}
    {% javascript %}
        z_growl_add("Je bent nu lid van deze kennisgroep");
    {% endjavascript %}
{% endif %}

{% if q.qstatus=="leave-group" %}
    {% javascript %}
        z_growl_add("Je hebt de kennisgroep verlaten");
    {% endjavascript %}
{% endif %}
