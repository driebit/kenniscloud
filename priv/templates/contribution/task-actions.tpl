{% if id.is_editable and not id.is_completed %}
    {% if id.is_a.task %}
        {% button
            text=_"Mark task as done"
            class="btn c-btn--orange"
            action={script script="$('#save_stay').click();"}
            action={dialog_open
                template="contribution/task-action-dialog-done.tpl"
                title=_"Are you sure?"
                id=id
                dispatch_to=dispatch_to
            }
        %}
    {% elseif id.is_a.contribution %}
        {% button
            text=_"Convert to a task"
            class="btn c-btn--orange"
            action={script script="$('#save_stay').click();"}
            action={dialog_open
                template="contribution/task-action-dialog-convert.tpl"
                title=_"Are you sure?"
                id=id
                dispatch_to=dispatch_to
            }
        %}
    {% endif %}
{% endif %}
