<!DOCTYPE html>
<!--
#################################
      _      _      _     _ _
   __| |_ __(_) ___| |__ (_) |_
  / _` | '__| |/ _ \ '_ \| | __|
 | (_| | |  | |  __/ |_) | | |_
  \__,_|_|  |_|\___|_.__/|_|\__|

############ driebit ############

 geavanceerde internetapplicaties

        Oudezijds Voorburgwal 282
                1012 GL Amsterdam
                   020 - 420 8449
                  info@driebit.nl
                   www.driebit.nl

#################################
//-->
<html lang="{{ z_language }}" class="environment-{{ m.site.environment }}" {% block html_attr %}{% endblock %}>
    <head>
        <meta charset="utf-8" />
        <title>
            {% block title %}
            {% if id %}
                {{ id.seo_title|default:id.title ++ " - " ++ m.site.title }}
            {% else %}
                {{ m.site.title }}
            {% endif %}
            {% endblock %}
        </title>

        {% block favicon %}
            <link rel="icon" href="/favicon.ico" type="image/x-icon" />
            <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
        {% endblock %}

        <link rel="manifest" href="{% url manifest_json %}" />

        {% include "head/twitter.tpl" %}
        {% include "head/matomo.tpl" %}

        <meta name="viewport" content="width=device-width, initial-scale=1.0" />

        {% block _html_head %}
            {% all include "_html_head.tpl" %}
            {% block html_head_extra %}{% endblock %}
        {% endblock %}
    </head>

    <body class="{% if m.rsc[id.content_group_id] %}has-contentgroup {% endif %} {% block body_class_category %}{{ id.category.name }}{% endblock %} {% block body_class %}{% endblock %}" {% block body_attrs %}{% endblock %} data-cotonic-pathname-search="{% cotonic_pathname_search %}">
        {% block global_nav %}
            {% catinclude "global-nav/global-nav.tpl" id %}
        {% endblock %}

        {% block content_group_nav %}
            {% include "content-group-nav/content-group-nav.tpl" %}
        {% endblock %}

        {% block content_area %}
            {% block content %}{% endblock %}
        {% endblock %}

        {% block footer %}
            {% include "footer/footer.tpl" %}
        {% endblock %}

        {% all include "_html_body.tpl" %}

        {% include "_js_include.tpl" %}
        {% script %}

        {% worker name="auth" src="js/zotonic.auth.worker.js" args=%{ auth: m.authentication.status } %}
    </body>
</html>
