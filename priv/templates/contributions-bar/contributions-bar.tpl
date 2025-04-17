<div class="contributions-bar">
    {% if not m.acl.user %}
    <div>
        <p>
            Als je je aanmeldt bij KennisCloud kun je jouw kennis delen met anderen
            en vind je mensen die kennis hebben waar jij naar op zoek bent. Je
            krijgt wekelijkse updates, zodat je op de hoogte blijft van thema’s die
            jou interesseren en bijeenkomsten waar je naartoe kunt gaan.
        </p>
        <a href="{% url logon p={page id=id}|url %}" class="btn--primary">
            Inloggen/registreren
        </a>
    </div>

    {% else %}
    <div>
        <p>
            Stel een vraag, start een gesprek of deel interessante links,
            kennis of inspiratie
        </p>
        <a href="{% url contribution_edit kennisgroep=id.content_group_id auteur=m.acl.user %}" class="btn--primary -icon">
            Bijdrage plaatsen
        </a>
    </div>

    <div>
        <p>
            Organiseer een bijeenkomst. Kun je hier hulp bij gebruiken?
            Neem dan contact op met de {% if id.o.hascollabmanager.id.o.hasusergroup.name == "acl_user_group_project_manager" %}projectleider{% else %}Community Librarian{% endif %} van deze kennisgroep.
        </p>
        
        <a href="{% url meetup_edit kennisgroep=id.content_group_id auteur=m.acl.user %}" class="btn--primary -icon">
            Organiseer een meetup
        </a>
    </div>
{% endif %}
</div>
