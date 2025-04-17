<div class="{% block cta_class %}cta{% endblock %}">
	<h2>
		{% block cta_title %}
			Help de kennis groeien, doe mee!
		{% endblock %}
	</h2>

    {% live
        template="page-actions/actions.tpl"
        catinclude
        id=id
        topic={subject id=m.acl.user predicate=`hascollabmember`}
    %}
</div>
