{% with nolink|default:"false" as nolink %}
	{% live
		template="meta/show-likes.tpl"
		nolink=nolink
		topic={subject id=id predicate=`like`}
		id=id
	%}
{% endwith %}