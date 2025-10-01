{% if id.has_mentilink|is_defined %}
    {% with id.has_mentilink as mentilink %}
        {% if mentilink|slice:[,4] == "app/" %}
            <div class="c-external-services__mentimeter"><iframe sandbox='allow-forms allow-scripts allow-same-origin allow-presentation' allowfullscreen='true' allowtransparency='true' frameborder='0' height='315' src='https://www.mentimeter.com/{{ mentilink }}/embed' style='position: absolute; top: 0; left: 0; width: 100%; height: 100%;' width='420'></iframe></div>
        {% else %}
            <div class="c-external-services__mentimeter"><iframe sandbox='allow-forms allow-scripts allow-same-origin allow-presentation' allowfullscreen='true' allowtransparency='true' frameborder='0' height='315' src='https://www.menti.com/{{ mentilink }}' style='position: absolute; top: 0; left: 0; width: 100%; height: 100%;' width='420'></iframe></div>
        {% endif %}
    {% endwith %}
{% endif %}
