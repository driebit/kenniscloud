{# Render link to logon/signup when the user is unregistered and not on a logon/signup page #}
{% if
    zotonic_dispatch != `logon` and
    zotonic_dispatch != `logon_change` and
    zotonic_dispatch != `logon_reset` and
    zotonic_dispatch != `logon_reminder` and
    zotonic_dispatch != `logon_confirm` and
    zotonic_dispatch != `signup` and
    zotonic_dispatch != `signup_confirm` and
    not m.acl.user
%}
    <a class="login--global-nav" href="{% url logon p=`home`|url %}" title="{_ logon/signup _}">
        <i class="icon--person"></i> <span>{_ logon/signup _}</span>
    </a>
{% endif %}
