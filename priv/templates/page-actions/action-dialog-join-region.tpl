{% if m.acl.user as user %}
    {% if leave|default:false %}
        <p>
           Je ontvangt dan geen updates meer uit deze regio.
           {# would be nice to query for kennisgroepen from this region to tailor the message below #}
           Eventueel wel van kennisgroepen uit deze regio waar je nog lid van bent.
        </p>
        <div class="main-aside__view">
            {% button class="btn--primary -icon" text=_"Ja, helaas"
                      postback={leave target=id.id} delegate=`kenniscloud`
                      action={dialog_close}
            %}
        </div>
    {% else %}
        <p>
            Je ontvangt dan updates over deze regio via email
            (afhankelijk van de mail-instellingen op je <a href="{{user.page_url}}">profielpagina</a>).
        </p>
        <div class="main-aside__view">
            {% button class="btn--primary -icon" text=_"Ja, ik wil de regio volgen"
                      postback={join target=id.id} delegate=`kenniscloud`
                      action={dialog_close}
            %}
        </div>
    {% endif %}
{% else %}
    <p>
        Om dit te doen moet je ingelogd zijn.
    </p>
{% endif %}
