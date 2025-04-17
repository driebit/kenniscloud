{% if m.acl.user and id.category_id|member:m.predicate.object_category.flag %}
    {% live
        template="page-actions/page-action-connect-user.tpl"
        topic={subject id=id predicate=`flag`}
        id=id
        predicate='flag'
        btn_connect_text=_"<i class='icon--flag'></i><span>Meld deze bijdrage als ongepast</span>"
        btn_cancel_text=_"<i class='icon--flag'></i><span>Aangemeld als ongepast - melding intrekken</span>"
        btn_class="btn--flag"
        can_connect=can_connect
    %}
{% endif %}
