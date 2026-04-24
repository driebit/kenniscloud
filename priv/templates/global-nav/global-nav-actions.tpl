{% include "toggle-menu/toggle-menu.tpl" %}
<div class="global-nav__meta">
    {% if m.acl.user as user %}
        <div class="global-nav__notifications">
            {% live template="dialog-notifications/dialog-notifications.tpl" topic=["bridge", "origin", "user", m.acl.user, "activities"] %}
        </div>
    {% endif %}
    <div class="global-nav__actions cf">
        {% include "_auth_link.tpl" %}
        {% include "dialog-profile/button-profile.tpl" %}
        {% include "dialog-language/button-language.tpl" raw_path=m.req.raw_path %}
    </div>
</div>
{% include "search-suggestions/search.tpl" identifier="global-nav" %}
