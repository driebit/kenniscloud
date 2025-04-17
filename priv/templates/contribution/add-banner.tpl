{% if collab_group.s.has_subgroup and not id.is_temporary %}
    <div class="row">
        <div class="form-group col-xs-12">
            <div>
                <label class="control-label">Banner toevoegen (optioneel)</label>
                <p class="control-label-summary">
                    Voeg een afbeelding toe bij je bijdrage. Deze afbeelding wordt onder je titel in het groene vlak geplaatst.
                </p>
            </div>
            {% include "_ginger_connection_widget.tpl" predicate_ids=[m.rsc.hasbanner.id] direction="out" %}
            {% if id.hasbanner|length < max %}
                {% include "_action_ginger_connection.tpl" category=`image` predicate=`hasbanner` new_rsc_title=m.rsc.hasbanner.title tabs_enabled=["upload"] tab="upload" direction="out" dispatch="ginger_edit" %}
            {% endif %}
        </div>
    </div>
{% endif %}
