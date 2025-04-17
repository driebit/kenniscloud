<html lang="nl">
<head>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <title>Dagcrowd: Leuk dat je meedoet!</title>
    <base href="http://{{ m.site.hostname }}/" />
    <base target="_blank" />
</head>
<body style="box-sizing: border-box; line-height: 1.5; font-family: arial, sans-serif;">
    <div style="width: 100%; background: #F9F9F9;">
        <div style="box-sizing: border-box; width: 100%; max-width: 50em; margin: 0 auto; padding: 2em; background: #FFF;">
            <table width="100%" id="email-table" border="0" cellspacing="0" cellpadding="0">
                <tr>
                    <td>
                        <img src="/lib/images/mail-bibliotheek-logo.jpg" style="width: 15em; max-width: 60%;" alt="">
                    </td>
                </tr>
            </table>
            <div style="width: 100%; padding-bottom: 1.25em; border-bottom: 2px solid #00A2B6;">
                <p>Beste
                    {{ name|escape }},
                </p>
                <p>
                    Welkom in de DagCrowd van de Bibliotheek LocHal!
                </p>
                <p>
                    Met deze unieke link kun je de DagCrowd bekijken:
                    <br>
                    <a href="{{ link }}">{{ link }}</a>
                </p>
                <p>
                    Deze link is één keer geldig. Je kunt altijd een nieuwe link
                    aanvragen door je opnieuw te registreren.
                </p>
            </div>
        </div>
    </div>
</body>
</html>
