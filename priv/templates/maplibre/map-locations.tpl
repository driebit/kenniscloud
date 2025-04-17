{% for rid, lat, lng, cat in result %}
    {
        "type": "Feature",
        "properties": {
            "id": "{{ rid }}",
            "icon": "{{ cat.o.hasicon[1].medium.filename }}"
        },
        "geometry": {
            "type": "Point",
            "coordinates": ["{{ lng }}", "{{ lat }}", 0.0]
        }
    }
    {% if not forloop.last %},{% endif %}
{% endfor %}
