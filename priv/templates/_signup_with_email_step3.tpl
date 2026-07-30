{% overrules %}

{% block signup_form_fields_tos %}
    <p class="help-block">
        {_ We will be very careful with all the information given to us and will never give your name or address away without your permission. We do have some rules that we need you to agree with. _}
    </p>

    <div class="form-group" id="signup_tos">
        <div id="signup_tos_agree_group">
            <label class="checkbox" for="signup_tos_agree">
                <input type="checkbox" name="signup_tos_agree" id="signup_tos_agree" value="1" required>
                {_ I agree to the Terms of Service and the Privacy policies. _}
            </label>
            {% with m.rsc.signup_privacy.page_url as privacy_url %}
                {% if privacy_url %}
                    <p class="help-block">
                        {_ You can read them here: _}
                        {% if privacy_url %}
                            <a target="_blank" href="{{ privacy_url }}">{_ Privacy policies _} <span class="fa fa-external-link"></span></a>
                        {% endif %}
                    </p>
                {% endif %}
            {% endwith %}
            {% validate id="signup_tos_agree"
                type={acceptance failure_message=_"You must agree to the Terms in order to sign up."}
                message_after="signup_tos_agree_group"
            %}
        </div>
    </div>

    <div style="display: none" id="signup_error_tos_agree" class="signup-error">
        <p class="text-danger">
            {_ To sign up you must agree with the Terms of Service and Privacy policies. _}
        </p>
    </div>
{% endblock %}
