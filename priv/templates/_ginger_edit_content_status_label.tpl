<div class="ginger-edit__aside-item ginger-edit__aside {{ extraClass }}" id="ginger-edit__aside">

    <h3 class="section-title">{_ Progress Label _}</h3>
    <label class="control-label" for="progress-label">{_ Progress labels are visible on the knowledgegroup _}</label>
    <div class="form-group row ">
        {% with m.search[{query cat_exact=`status_keyword`}] as statuses %}
            <select id="progress-label" name="progress_label" class="form-control c-status-select">
                <option value="">{_ Select a progress label _}</option>
                {% for status in statuses %}
                    <option value="{{ status.id }}"
                        {% if id.progress_label == status.id %}selected{% endif %}>
                        {{ status.title|capfirst }}
                    </option>
                {% endfor %}
            </select>
        {% endwith %}
    </div>

    <h3 class="section-title">{_ Status Label _}</h3>
    <label class="control-label" for="status-label">{_ Status labels are visible on contributions and meetups _}</label>
    <div class="form-group row">
        <select id="status-label" name="status_label" class="form-control c-status-select">
            <option value="">{_ Select a status _}</option>
            <option value="Preparation" {% if id.status_label == "Preparation" %}selected{% endif %}>{_ Preparation _}</option>
            <option value="Meetup" {% if id.status_label == "Meetup" %}selected{% endif %}>{_ Meetup _}</option>
            <option value="Insights" {% if id.status_label == "Insights" %}selected{% endif %}>{_ Insights _}</option>
            <option value="Discussion" {% if id.status_label == "Discussion" %}selected{% endif %}>{_ Discussion _}</option>
        </select>
    </div>
</div>