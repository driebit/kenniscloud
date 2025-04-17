{% if id.category.is_a.task %}
    {% if id.is_completed %}
        <span class="list-item-kg__task {{extraClass}}"><img src="/lib/images/icon-done.svg" alt="checkmark icon" class="list-item-kg__status-icon" />{_ Completed task _}</span>
    {% else %}
        <span class="list-item-kg__task {{extraClass}}"><img src="/lib/images/icon-todo.svg" alt="unchecked checkmark icon" class="list-item-kg__status-icon" />{_ Incomplete task _}</span>
    {% endif %}
{% endif %}