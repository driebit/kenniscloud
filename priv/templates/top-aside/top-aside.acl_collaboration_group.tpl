<div class="u-d-flex u-flex-col u-flex-gap-2">
    {% if id|kc_collaboration_group_progress_label as result %}
        {% catinclude "keywords/progress-tags.tpl" id latest_contribution=result.id %} 
    {% endif %}

    {% live
        template="person/person-list.tpl"
        catinclude
        id=id
        show=11
        collab_group=collab_group
        title="Wij doen mee"
        topic={object id=id predicate=`hascollabmanager`}
        topic={object id=id predicate=`hascollabmember`}
    %}

    {% include "acl_collaboration_group/additional-content-buttons.tpl" %}


    {% if id.o.hasprojectinitiator as initiators %}
        <div class="project-initiators">
            <h3 class="bordered-title">Dit is een initiatief van</h3>
            <div class="project-initiators-grid">
                {% for initiator in initiators %}
                    <div class="initiator-logo__wrapper">
                        {% image initiator.depiction.id class="initiator-logo" %}
                    </div>
                {% endfor %}
            </div>
            <p>
                {# Temporary hardcoded link for MVP #}
                <a href="{{ m.rsc.vpro_waag_collaboration.page_url }}" class="kg-intro__right__link">Lees over de initiatiefnemers</a>
            </p>
        </div>
    {% endif %}
</div>
