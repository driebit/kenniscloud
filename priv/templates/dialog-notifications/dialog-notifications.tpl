<div id="elm-notifications" class="notifications">
</div>
{% javascript %}
    var notificationsElement = document.getElementById("elm-notifications");
    var now = Date.now();
    var app = Elm.Notifications.init({
        node: notificationsElement,
        flags: {
            alert: {{ m.kc_user.activity_inbox.alert }},
            id: {{ m.acl.user.id }},
            now: now
        }
    });
{% endjavascript %}
