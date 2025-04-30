{% if id.is_editable and not id.is_completed %}
    {% if id.is_a.task %}
        {% button
            text=_"Mark task as done"
            class="btn c-btn--orange"
            postback={complete_task id=id dispatch_to=dispatch_to|default:'page'}
            delegate=`kenniscloud`
        %}
    {% elseif id.is_a.contribution %}
        {% button
            class="btn c-btn--orange"
            text=_"Convert to a task"
            postback={contribution_to_task id=id dispatch_to=dispatch_to|default:'page'}
            delegate=`kenniscloud`
        %}
    {% endif %}
{% endif %}
