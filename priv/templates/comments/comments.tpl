<div id="page-id" data-id="{{ id.id }}">
	<h2 class="bordered-title">Reacties</h2>
	{% if not m.acl.user %}
		<p class="remarks-introduction"><a href="{% url logon p={page id=id}|url %}">log in of registreer</a> om een reactie te geven</p>
	{% endif %}
    <div id="elm-remarks"></div>
</div>

{% javascript %}
    var remarksElement = document.getElementById("elm-remarks");
    var now = Date.now();

    let hash;
    if (window.location.hash.slice(1)){
        hash = parseInt(window.location.hash.slice(1))
    } else {
        hash = null
    };

    //ocataco: last flag is passed to indicate this page is closed, and no new remarks are allowed
    var app = Elm.Main.init({
        node: remarksElement,
        flags: { 
                 pageId: {{ id }}, 
                 now: now, 
                 remarkId: hash, 
                 isClosed: {{id.status_label == 'Closed'}}
                 } 
    });

    app.ports.scrollIdIntoView.subscribe(function(domId) {
        document.getElementById(domId).scrollIntoView();
    });

    app.ports.logError.subscribe(e => console.error(e));
{% endjavascript %}
