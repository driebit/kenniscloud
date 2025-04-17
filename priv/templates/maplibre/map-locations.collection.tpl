{% for item in result %}
{% with item._source as record %}

{
    "type": "Feature",
    "properties": {
        "id": "{{ item._id }}",
        {% if item._type == "resource" %}
            "icon": "{{ m.rsc[record.category|first].o.hasicon[1].medium.filename }}"
        {% else %}
            "icon": "object"
        {% endif %}
    },
    "geometry": {
        "type": "Point",
        "coordinates": ["{{ record.geolocation.lon }}", "{{ record.geolocation.lat }}", 0.0]
    }
}

{% if not forloop.last %},{% endif %}
{% endwith %}
{% endfor %}
