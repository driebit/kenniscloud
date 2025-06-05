{# Note: 'r' is expected to be a map for a suggestion as described in 'kenniscloud_azb' #}
<li class="list-item-small">
    <div class="list-item-small__content">
        <small>Bibliotheekcollectie</small>

        <h3>{{ r['title'] }}</h3>

        <p>{{ r['creator'] }}</p>
        <p>{{ r['genre'] }} / {{ r['date'] }}</p>

        <a href="{{ r['uri'] }}" target="_blank">Lees meer over deze titel</a>
    </div>
    {% button text="Toevoegen"
        class="btn--dark"
        delegate=`kenniscloud_suggestion`
        postback={confirm_suggestion
            subject=id
            uri=r['uri']
        }
    %}
</li>
