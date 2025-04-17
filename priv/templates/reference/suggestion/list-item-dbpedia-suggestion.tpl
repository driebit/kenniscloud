<li class="list-item-small">
    <div class="list-item-small__img">
        <img src="{{ r.thumbnail|https }}">
    </div>
    <div class="list-item-small__content">
        <small>Wikipedia</small>

        <h3>{{ r.title|default:r['http://nl.dbpedia.org/property/naam'] }}</h3>

        <p>{{ r.abstract|truncate:100 }}</p>

        <a href="{{ r['http://xmlns.com/foaf/0.1/isPrimaryTopicOf']|https }}">Lees meer op Wikipedia</a>
    </div>
    {% button text="Toevoegen"
        class="btn--dark"
        delegate=`kenniscloud_suggestion`
        postback={confirm_suggestion
            subject=id
            uri=r.uri
        }
    %}
</li>
