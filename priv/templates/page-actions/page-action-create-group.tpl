{% if m.acl.authenticated.insert.acl_collaboration_group %}
    {% with m.acl.user as user %}
    {% with btn_title|default:"Start je eigen kennisgroep" as btn_title %}
    {% with btn_class|default:"btn--secondary" as btn_class %}

        <a id="{{ #create_group }}" href="#connect"
            class="{{ btn_class }}">
            {{ btn_title }}
        </a>

        {% wire id=#create_group
            action={dialog_open
                template="page-actions/action-dialog-create-group.tpl"
                logon_required
                title=_"Maak een nieuwe kennisgroep"
                id=id
                user=user
                redirect="#reload"
                use_wire
            }
        %}

    {% endwith %}
    {% endwith %}
    {% endwith %}
{% endif %}
