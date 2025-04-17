<html lang="nl">
<head>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <title>KennisCrowd: Leuk dat je erbij was!</title>
    <base href="http://{{ m.site.hostname }}/" />
    <base target="_blank" />
</head>
<body style="box-sizing: border-box; line-height: 1.5; font-family: arial, sans-serif;">
    <div style="width: 100%; background: #F9F9F9;">
        <div style="box-sizing: border-box; width: 100%; max-width: 50em; margin: 0 auto; padding: 2em; background: #FFF;">
            {% with m.crowd[crowd].is_daycrowd as is_daycrowd %}
            <table width="100%" id="email-table" border="0" cellspacing="0" cellpadding="0">
                <tr>
                    <td>
                        {% if is_daycrowd %}
                        <img src="/lib/images/mail-bibliotheek-logo.jpg" style="width: 15em; max-width: 60%;" alt="">
                        {% else %}
                        <img src="/lib/images/mail-kc-logo.png" style="width: 15em; max-width: 60%;" alt="">
                        {% endif %}
                    </td>
                </tr>
            </table>
            <div style="width: 100%; padding-bottom: 1.25em; border-bottom: 2px solid #00A2B6;">
                <p>Beste
                    {{ name|escape }},
                </p>
                {% if is_daycrowd %}
                    <p>
                        Bedankt dat je deelnam aan de "{{ crowd.title }}" DagCrowd!
                        We zijn benieuwd wat je er van vond.
                        Je helpt ons erg door deze korte vragenlijst in te vullen:
                        <a href="https://forms.office.com/Pages/ResponsePage.aspx?id=b4qtILAx3kSM6QU2CbmO9UVdOy3pr29PjloX47E_cxZUNEJHSFA5Q0tVNVpHRDk4UEgzWENTS1hLNy4u">
                            Vragen na deelname DagCrowd
                        </a>
                    </p>
                    <p>
                        De persoonsgegevens die je opgaf om deel te nemen aan de DagCrowd
                        werden enkel voor de DagCrowd gebruikt en zijn verwijderd.
                    </p>
                    <p>
                        Kijk voor gesprekken en meetups over actuele onderwerpen gerust verder op
                        <a href="https://www.kenniscloud.nl">www.kenniscloud.nl</a>.
                    </p>
                {% else %}
                    <p>
                        Je ontvangt deze mail omdat je deelnam aan een KennisCrowd meetup.
                        De persoonsgegevens die je daarvoor opgaf, werden tijdelijk bewaard en zijn verwijderd.
                    </p>
                    <p>
                        Leuk dat je erbij was!
                    </p>
                    <p>
                        Het gesprek gaat door op KennisCloud! Wil je meepraten over dit thema, en op de hoogte blijven van activiteiten? Meld je dan aan via:
                    </p>
                    <p>
                      <a href="{% url signup absolute_url %}">{% url signup absolute_url %}</a>
                    </p>
                {% endif %}
            </div>
            <p>
                Je ontvangt eenmalig dit mailbericht, omdat je hebt meegedaan aan een KennisCrowd meetup en je je
                opgegeven persoonsgegevens na het einde van de meetup wilde laten verwijderen.
                Ook de registratie van het email-adres waar dit bericht aan is gericht, is verwijderd uit ons systeem.
            </p>
            {% endwith %}
        </div>
    </div>
</body>
</html>
