<div id="signup_region_group" class="form-group">
    <label for="region" class="control-label">Aanmelden voor regio</label>
    <div>
        <select id="signup_region" name="signup_region" class="form-control">
        	<option value="">{_ Select _}</option>
            {% with m.rsc[`region_none`] as region_none %}
                {% with m.search[{query cat_exact=`region` id_exclude=region_none.id sort="rsc.pivot_title"}] as regions %}
                    {% for region in regions %}
                	   <option value="{{ region.name }}"{% if id.region == "{{ region.name }}" %} selected{% endif %}>{{ region.title }}</option>
                    {% endfor %}
                {% endwith %}
                <option value="{{ region_none.name }}">{{ region_none.title }}</option>
            {% endwith %}
        </select>
    </div>
    {% validate id="signup_region"
        type={presence failure_message="Selecteer een regio of kies de optie \"Geen regio\""}
    %}
</div>
