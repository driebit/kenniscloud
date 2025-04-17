{% if id.o.subject %}
    <p>
        <a href="/edit/{{ m.rsc[id].id }}">Koppel andere tags</a> aan deze bijdrage, dan heb
        je meer kans op suggesties uit de bibliotheekcatalogus.
    </p>
{% else %}
    <p>
        <a href="/edit/{{ m.rsc[id].id }}">Voeg tags toe</a> aan deze bijdrage om suggesties
        uit de bibliotheekcollectie te krijgen.
    </p>
{% endif %}
