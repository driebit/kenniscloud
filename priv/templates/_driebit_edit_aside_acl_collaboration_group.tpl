<div class="ginger-edit__aside">
    <h3>{_ Options _}</h3>
    <h5>{_ Map _}</h5>
    <div class="form-group row ">
        <label class="control-label col-md-12" for="has_map">
            <input type="checkbox" id="has_map" name="has_map"
                value="1"
                {% if id.has_map %}checked{% endif %}
                {% if not id.is_editable %}disabled="disabled"{% endif %} />
            {_ Kaart opnemen _}
        </label>
    </div>

    <h5>{_ Timeline _}</h5>

    <div class="form-group row">
        <label class="control-label col-md-12" for="timeline_off">
            <input type="radio" id="timeline_off" name="timeline_status"
                value="off"
                {% if id.timeline_status == "off" or not id.timeline_status %}checked{% endif %}/>
            {_ Timeline off _}
        </label>
        <label class="control-label col-md-12" for="timeline_automatic">
            <input type="radio" id="timeline_automatic" name="timeline_status"
                value="automatic"
                {% if id.timeline_status == "automatic" %}checked{% endif %}/>
            {_ Automatic timeline _}
        </label>
        <label class="control-label col-md-12" for="activate_timeline">
            <input type="radio" id="activate_timeline" name="timeline_status"
                value="manual"
                {% if id.timeline_status == "manual" %}checked{% endif %}/>
            {_ Manual timeline _}
        </label>

        <div class="control-label">
            <h6 class="col-md-12">{_ Sorting order _}</h5>
            <label class="control-label col-md-12" for="timeline_asc">
                <input type="radio" id="timeline_asc" name="timeline_order"
                    value="asc"
                    {% if id.timeline_order == "asc" or not id.timeline_order %}checked{% endif %}/>
                {_ Ascending _}
            </label>
            <label class="control-label col-md-12" for="timeline_desc">
                <input type="radio" id="timeline_desc" name="timeline_order"
                    value="desc"
                    {% if id.timeline_order == "desc" %}checked{% endif %}/>
                {_ Descending _}
            </label>
        </div>

        {% if m.acl.use.mod_admin %}
            <h4 style="margin-top: 40px;">{_ Mentimeter and OpnForm _}</h4>
            <label class="control-label col-md-12" for="has_menti">
                <input type="checkbox" id="has_menti" name="has_menti"
                    value="1"
                    {% if id.has_menti %}checked{% endif %}
                    {% if not id.is_editable %}disabled="disabled"{% endif %} />
                {_ Mentimeter toevoegen _}
            </label>
            <input type="text" id="has_mentilink" name="has_mentilink" value="{{ id.has_mentilink }}" placeholder="aloc6zmqtnfh" class="control-label col-md-12"
                    {% if not id.is_editable %}disabled="disabled"{% endif %} />
            <label class="control-label col-md-12" for="has_mentilink">
                {_ Mentimeter embed url or code _}
                <span class="help-block">{_ to include a presentation you need to use the embed code in the following format _}: "app/presentation/al3bdpfthdpo9h5dwqjoch24ej6ofskc"</span>
            </label>

            <label class="control-label col-md-12" for="has_opnform">
                <input type="checkbox" id="has_opnform" name="has_opnform"
                    value="1"
                    {% if id.has_opnform %}checked{% endif %}
                    {% if not id.is_editable %}disabled="disabled"{% endif %} />
                {_ OpnForm toevoegen _}
            </label>
            <input type="text" id="has_opnformlink" name="has_opnformlink" value="{{ id.has_opnformlink }}" placeholder="customer-feedback-survey-9l6tuv" class="control-label col-md-12"
                    {% if not id.is_editable %}disabled="disabled"{% endif %} />
            <label class="control-label col-md-12" for="has_opnformlink">
                {_ OpnForm embed url _}
            </label>
        {% endif %}

    </div>
</div>
