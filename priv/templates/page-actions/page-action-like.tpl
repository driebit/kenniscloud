{% if id.category_id|member:m.predicate.object_category.like %}
    {% live
        template="page-actions/page-action-connect-user.tpl"
        topic={subject id=id predicate=`like`}
        id=id
        predicate=`like`
        btn_connect_text=_"Waarderen"
        btn_cancel_text=_" "
        btn_class="btn--like"
        can_connect=can_connect
    %}
{% endif %}
