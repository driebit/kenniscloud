<li class="list-item-small">
    {% if not add %}
        <a href="https://www.bibliotheek.nl/catalogus/titel.{{ r['ppn'] }}.html" target="_blank">
    {% endif %}
    <div class="list-item-small__content">
        <small>Bibliotheekcollectie</small>

        <h3>{{ r['dcterms:title'] }}</h3>

        <p>{{ r['dcterms:creator']|join:", " }}</p>
        <p>{{ r['schema:genre']|join:", " }} / {{ r['dcterms:date'] }}</p>

        {% if add %}
            <a href="https://www.bibliotheek.nl/catalogus/titel.{{ r['ppn'] }}.html" target="_blank">Lees meer over deze titel</a>
        {% else %}
            <span>Lees meer over deze titel</span>
        {% endif %}
    </div>
    {% if not add %}
        </a>
    {% endif %}
    {% if add %}
        {% button text="Toevoegen"
            class="btn--dark"
            delegate=`kenniscloud_suggestion`
            postback={confirm_suggestion
                subject=id
                uri=r.uri
            }
        %}
    {% endif %}
</li>
