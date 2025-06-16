<div class="ginger-edit__aside-item ginger-edit__aside" id="ginger-edit__aside">

    <h3 class="section-title">{_ Status Label _}</h3>
    <div class="form-group row {{ extraClass }}">
        <select id="status-label" name="status_label" class="form-control c-status-select">
            <option value="">{_ Select a status _}</option>
            <option value="Preparation" {% if id.status_label == "Preparation" %}selected{% endif %}>{_ Preparation _}</option>
            <option value="Meetup" {% if id.status_label == "Meetup" %}selected{% endif %}>{_ Meetup _}</option>
            <option value="Insights" {% if id.status_label == "Insights" %}selected{% endif %}>{_ Insights _}</option>
            <option value="Discussion" {% if id.status_label == "Discussion" %}selected{% endif %}>{_ Discussion _}</option>
        </select>
    </div>

</div>