<li class="list-item-small">
    <a href="{{ r['http://xmlns.com/foaf/0.1/isPrimaryTopicOf']|https }}" target="_blank">
        <div class="list-item-small__img">
            {% if r.thumbnail|is_list %}
                <img src="{{ r.thumbnail|first|https }}">
            {% else %}
                <img src="{{ r.thumbnail|https }}">
            {% endif %}

        </div>
        <div class="list-item-small__content">
            <small>Wikipedia</small>

            {% if r.title|is_list %}
                <h3>{{ r.title|first }}</h3>
            {% else %}
                <h3>{{ r.title|default:r['http://nl.dbpedia.org/property/naam'] }}</h3>
            {% endif %}

            <p>{{ r.abstract|truncate:100 }}</p>

            <span>Lees meer op Wikipedia</span>
        </div>
    </a>
</li>
