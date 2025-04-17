<form id="emailpreferences" method="post" action="postback" class="emailpreferences">
    <fieldset>
        <p class="emailpreferences-group">
            <input type="checkbox" name="receive_weekly_update_mail" id="receive_weekly_update_mail" value="1" {% if id.receive_weekly_update_mail|if_undefined:true %}checked{% endif %}>
            <i class="emailpreferences-group__checkbox"></i>
            <label for="receive_weekly_update_mail">
                Ik ontvang graag wekelijks de Update, met hierin een aantal recente bijdragen op KennisCloud
            </label>
            <span>In deze mail attenderen we je op recente bijdragen in jouw kennisgroepen en de eerstvolgende meetup in jouw regio.</span>
        </p>
        {% if m.kc_user.is_community_librarian or m.acl.is_admin %}
            <p class="emailpreferences-group">
                <button id="admin_send_weekly_preview" class="btn btn-primary" type="submit">Stuur voorbeeld</button>
                {% wire id="admin_send_weekly_preview" postback={admin_send_weekly_preview} delegate=`kenniscloud` %}
            </p>
            <p class="help-block">
                Stuur een voorbeeld van de wekelijkse KennisCloud update mail naar je eigen mailadres, met de inhoud die de update zou hebben als ze nu verstuurd zou worden.
            </p>
        {% endif %}

        <p class="emailpreferences-group">
            <input type="checkbox" name="receive_notification_mail" id="receive_notification_mail" value="1" {% if id.receive_notification_mail|if_undefined:true %}checked{% endif %}>
            <i class="emailpreferences-group__checkbox"></i>
            <label for="receive_notification_mail">
                Ik ontvang graag notificaties per mail
            </label>
            <span>We brengen je op de hoogte als je getagd bent en van nieuwe reacties op een bijdrage waar jij aan deelneemt. We sturen je maximaal twee mails per dag.</span>
        </p>
    </fieldset>
</form>
{% wire id="emailpreferences" type="submit" postback={ emailpreferences } action={ growl text="Je instellingen voor mail zijn opgeslagen!" } delegate=`kenniscloud` %}
{% wire id="receive_weekly_update_mail" action= { submit } %}
{% wire id="receive_notification_mail" action= { submit } %}
