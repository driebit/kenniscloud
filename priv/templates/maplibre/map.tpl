{% if id.has_map %}
<h3 class="bordered-title">Op de kaart</h3>

<div class="c-map-container" id="map"></div>
{% javascript %}
    const map = new maplibregl.Map({
        container: 'map',
        style:
            'https://api.maptiler.com/maps/streets/style.json?key={{ m.config.site.maptiler_key.value|default:"Q7Tu4TuqU8Vq350zKiq9" }}',
        center: [4.9041, 52.3676],
        zoom: 7,
        scrollZoom: false
    });

    map.touchZoomRotate.enable();

    map.addControl(new maplibregl.NavigationControl());

    const points = JSON.parse(`{{ m.search[{query cat=['contribution', 'event'] unfinished_or_nodate content_group=id pagelen=3000}]|geojson|to_json }}`);

    function getPopup(id) {
        var popup = new maplibregl.Popup();
        cotonic.broker.call("bridge/origin/model/template/get/render/maplibre/map-card.tpl", { "id": id }).then(
            function(reply) {
                popup.setHTML(reply.payload.result);
            })
            .catch(function(e) {
                console.log("Error on call to " + call.topic, e);
            });

        return popup;
    }

    const bounds = new maplibregl.LngLatBounds();

    points.features.forEach(feature => {
        const coordinates = feature.geometry.coordinates;
        const popup = getPopup(feature.properties.id);

        new maplibregl.Marker()
            .setLngLat(coordinates)
            .setPopup(popup)
            .addTo(map);

        bounds.extend(coordinates);
    });

    if (!bounds.isEmpty()) {
        map.fitBounds(bounds, {
            padding: 50,
            linear: true,
            duration: 1000,
            maxZoom: 9
        });
    }
{% endjavascript %}
{% endif %}

