{% if m.acl.user as user %}
{% if not id|member:user.o.hasregion %}

    <a id="{{ #join_region }}" href="#connect"
        class="btn--primary -blue-border -icon">
        Volg deze regio
    </a>

    {% wire id=#join_region
        action={dialog_open
            template="page-actions/action-dialog-join-region.tpl"
            logon_required
            title="Wil je de regio " ++ id.title ++" volgen?"
            id=id
            user=user
        }
    %}

{% else %}

    <a id="{{ #leave_region }}" href="#connect"
        class="btn--dark -icon">
        Deze regio niet meer volgen
    </a>

    {% wire id=#leave_region
        action={dialog_open
            template="page-actions/action-dialog-join-region.tpl"
            leave
            title="Wil je de regio " ++ id.title ++ " niet meer volgen?"
            id=id
            user=user
        }
    %}

{% endif %}
{% endif %}

{% print q.qstatus %}
{% if q.qstatus=="join-region" %}
    {% javascript %}
        z_growl_add("Je volgt nu deze regio");
    {% endjavascript %}
{% endif %}

{% if q.qstatus=="leave-region" %}
    {% javascript %}
        z_growl_add("Je volgt de regio niet meer");
    {% endjavascript %}
{% endif %}
