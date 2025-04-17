<div id="elm-tips"></div>

{% javascript %}
    var tipsElement = document.getElementById("elm-tips");

    var app = Elm.Tips.init({
        node: tipsElement,
        flags: {{ id }}
    });
{% endjavascript %}
