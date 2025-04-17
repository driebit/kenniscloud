<aside>

    {% if id.is_editable %}
        <div class="form-group row ">
            <label class="control-label col-md-12" for="has_map">
                <input type="checkbox" id="has_map" name="has_map"
                    value="1"
                    {% if id.has_map %}checked{% endif %}
                    {% if not id.is_editable %}disabled="disabled"{% endif %} />
                {_ Kaart opnemen _}
            </label>
        </div>

        {% include "_ginger_edit_content_activate_timeline.tpl" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="image" predicate="hasbanner" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="keyword" predicate="subject" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="person" predicate="hascollabmember" title=_'Lid' tabs_enabled=["find"] tab="find" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="person" predicate="hascollabmanager" title=_'Beheerder' tabs_enabled=["find"] tab="find" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="acl_collaboration_group" predicate="has_subgroup" title=_'Subgroup' tabs_enabled=["find"] tab="find" %}

    {% endif %}

</aside>
