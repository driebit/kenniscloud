{% extends "page.tpl" %}

{% block title %}Aanmelden bij de dagcrowd{% endblock %}

{% block content %}
<div class="padding">
		<div id="verify-checking">
			<h1>Aanmelden bij de dagcrowd</h1>

			<p><img src="/lib/images/spinner.gif" width="16" height="16" />Een moment alstublieft...</p>
		</div>

		{% wire postback={confirm participant=q.participant} delegate=`mod_crowdparticipant` %}
</div>
{% endblock %}
