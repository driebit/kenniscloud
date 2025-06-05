<div class="ginger-edit__aside-item ginger-edit__aside" id="ginger-edit__aside">

    <h3 class="section-title">{_ Status _}</h3>
    <div class="form-group row ">
        {% with m.search[{query cat_exact=`status_keyword`}] as statusus %}
            {% for status in statusus %}
                <div style="display: flex; gap: 1rem; margin-top: 1rem; margin-left: 2rem;">
                    <input type="radio" id="{{ status.id }}" name="status"
                        value="{{ status.id }}"
                        {% if id.timeline_status == "{{ status.id }}" %}checked{% endif %}
                    />
                    <label class="control-label col-md-12" for="status-{{ status.id }}">
                        <div class="kg-intro_status kg-intro_status--{{ status.name }}">
                            <span>{{ status.title }}</span>
                        </div>
                    </label>
                </div>
            {% endfor %}
        {% endwith %}
    </div>
</div>