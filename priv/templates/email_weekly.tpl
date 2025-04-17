<html lang="nl">
<head>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    {% with news|filter:`short_title` as headlines %}
    <title>
        {% for headline in headlines %}{{ headline.short_title }} - {% endfor %}Jouw wekelijkse KennisCloud update
    </title>
    {% endwith %}
    <base href="http://{{ m.site.hostname }}/" />
    <base target="_blank" />
</head>
<body style="box-sizing: border-box; line-height: 1.5; font-family: arial, sans-serif;">
    <div style="width: 100%; background: #F9F9F9;">
        <div style="box-sizing: border-box; width: 100%; max-width: 50em; margin: 0 auto; padding: 2em; background: #FFF;">
            <table width="100%" id="email-table" border="0" cellspacing="0" cellpadding="0">
                <tr>
                   <td>
                       <img src="/lib/images/mail-kc-logo.png" style="width: 15em; max-width: 60%;" alt="">
                   </td>
               </tr>
               <tr height="12"></tr>
                 <tr>
                    <td>
                        <img src="/lib/images/mail-header.png" style="width: 100%;" alt="">
                    </td>
                </tr>
            </table>
            <div style="width: 100%; padding-bottom: 1.25em; border-bottom: 2px solid #00A2B6;">
                <p>Beste
                    {{ m.rsc[user_id].name_first|default:m.rsc[user_id].title }},
                </p>
                <p>Hier is de wekelijkse update van activiteit en nieuws in jouw kennisgroepen.</p>
            </div>
            {% if news %}
            <ul style="list-style: none; padding: 0; margin: 0;">
                {% for r in news %}
                    <li style="list-style: none; margin-left: 0; padding: 1.25em 0; border-bottom: 2px solid #00A2B6;">
                        <strong>
                            {{ (r.content_group_id.name == "default_content_group")|if:"KennisCloud":r.content_group_id.title }}
                        </strong>
                        <p style="margin: 0;">
                            <a style="color:  #00A2B6 !important; font-size: 1.4em;" href="{{ r.page_url }}">
                                {{ r.title }}
                            </a>
                        </p>
                        <p style="margin-top: 0; margin-right: 0; margin-bottom: .25em; margin-left: 0;">
                            {{ r.publication_start|date:"l j F"|capfirst }}
                        </p>
                        {{ r.summary }}
                    </li>
                    {% endfor %}
            </ul>
            {% endif %}
            {% if updates %}
            <ul style="list-style: none; padding: 0; margin: 0;">
                {% for r in updates %}
                    <li style="list-style: none; margin-left: 0; padding: 1.25em 0; border-bottom: 2px solid #00A2B6;">
                        <strong>
                            {{ r.content_group_id.title }}
                        </strong>
                        <p style="margin: .25em 0;">
                            Nieuwe {{ (r.category.name == "event")|if:"meetup":"bijdrage" }} van {{ r.creator_id.title }}:
                        </p>
                        <a style="color:  #00A2B6 !important; font-size: 1.4em;" href="{{ r.page_url }}">
                            {{ r.title }}
                        </a>
                        <p style="margin: .25em 0;">
                            {% if r.category.name == "event" %}
                               {% with
                                    (r.date_start > r.created)|if:(r.date_start|date:"l j F"):("gepost op " ++ (r.publication_start|default:r.created|date:"l j F")),
                                    r.address_title|if:(r.address_title ++ ", "):"",
                                    r.address_city|if:(r.address_city ++ ", "):""
                                  as eventdate, eventtitle, eventcity
                                %}
                                    {{ (eventtitle ++ eventcity ++ eventdate)|capfirst }}
                                {% endwith %}
                            {% else %}
                                {{ r.publication_start|default:r.created|date:"l j F"|capfirst }}
                            {% endif %}
                        </p>
                    </li>
                {% endfor %}
            </ul>
            {% endif %}
            {% if meetup_next as highlight %}
            <p style="margin: .5em 0;"><strong>Komende week in jouw regio:</strong></p>
            <div style="width: 100%; padding: 1.25em 0; background: #EAE8E8;">
                <p style="margin: .25em 1.5em;">
                    <strong>
                        {{ highlight.content_group_id.title }}
                    </strong>
                </p>
                <p style="margin: .25em 1.5em;">
                    <a style="color:  inherit !important; font-size: 1.4em;" href="{{ highlight.page_url }}">
                        {{ highlight.title }}
                    </a>
                </p>
                <p style="margin: .25em 1.5em;">
                    <span style="font-size: 1.4em;">
                        {% with
                            highlight.date_start|date:(highlight.date_is_all_day|if:"l j F":"l j F H:i"),
                            highlight.address_title|if:(highlight.address_title ++ ", "):"",
                            highlight.address_city|if:(highlight.address_city ++ ", "):""
                            as eventdate, eventtitle, eventcity
                        %}
                            {{ (eventtitle ++ eventcity ++ eventdate)|capfirst }}
                        {% endwith %}
                    </span>
                </p>
            </div>
            {% endif %}
            {% if meetup_upcoming as highlight %}
            <p style="margin: .5em 0;"><strong>Volgende maand in jouw regio:</strong></p>
            <div style="width: 100%; padding: 1.25em 0; background: #EAE8E8;">
                <p style="margin: .25em 1.5em;">
                    <strong>
                        {{ highlight.content_group_id.title }}
                    </strong>
                </p>
                <p style="margin: .25em 1.5em;">
                    <a style="color:  inherit !important; font-size: 1.4em;" href="{{ highlight.page_url }}">
                        {{ highlight.title }}
                    </a>
                </p>
                <p style="margin: .25em 1.5em;">
                    <span style="font-size: 1.4em;">
                        {% with
                            highlight.date_start|date:(highlight.date_is_all_day|if:"l j F":"l j F H:i"),
                            highlight.address_title|if:(highlight.address_title ++ ", "):"",
                            highlight.address_city|if:(highlight.address_city ++ ", "):""
                            as eventdate, eventtitle, eventcity
                        %}
                            {{ (eventtitle ++ eventcity ++ eventdate)|capfirst }}
                        {% endwith %}
                    </span>
                </p>
            </div>
            {% endif %}
            <p>
                Je ontvangt een wekelijkse mail om je te informeren over voor jou relevante toevoegingen aan KennisCloud.
                Dit zijn toevoegingen aan kennisgroepen waar jij bij aangesloten bent en meetups in jouw regio.
                Schrijf je <a href="{{ unsubscribe_url }}">hier</a> uit voor deze mail.
            </p>
        </div>
    </div>
</body>
</html>
