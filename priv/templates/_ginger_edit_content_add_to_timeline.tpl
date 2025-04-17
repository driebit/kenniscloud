<div class="ginger-edit__aside-item ginger-edit__aside--publicationdate" id="ginger-edit__aside--publicationdate">

    <h3>{_ Add to timeline _}</h3>
    <div class="form-group row ">
        <label class="control-label col-md-12" for="included_in_timeline">
            <input type="checkbox" id="included_in_timeline" name="included_in_timeline"
                value="1"
                {% if id.included_in_timeline %}checked{% endif %}
                {% if not id.is_editable %}disabled="disabled"{% endif %} />
            {_ This will add the item to the timeline of it's kennisgroep. _}
        </label>
    </div>
</div>