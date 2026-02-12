<aside>
    <a id="{{ #create_group }}" href="#connect"
       class="page-action--remove ">
        Verwijder je account
    </a>

    {% wire id=#create_group
            action={dialog_open
                    template="page-actions/action-dialog-delete-profile.tpl"
                    title=_"Verwijder je account"
                    id=id
                    user=user
                    redirect="/"
            }
    %}

    {% if m.rsc["hasbanner"] %}
        {% include "aside-connection/aside-add-connection.tpl" id=id cat="image" predicate="hasbanner" tab="upload" tabs_enabled=["upload","find"] %}
    {% endif %}

    {% if m.rsc["hasprofilepicture"] %}
        {% include "aside-connection/aside-add-connection.tpl" id=id cat="image" predicate="hasprofilepicture" tab="upload" tabs_enabled=["upload","find"] %}
    {% endif %}

    {% include "aside-connection/aside-add-connection.tpl" id=id cg_id=m.rsc.cg_user_generated.id cat="keyword" predicate="subject" tabs_enabled=["new","find"] tab="find" helper_text_top="Voeg tags toe van je interesses en expertises zodat je relevante connecties kunt maken." %}

</aside>
