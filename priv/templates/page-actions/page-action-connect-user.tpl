{% with can_connect|if_undefined:true as can_connect %}
{% with btn_class|default:"page-action--add" as btn_class %}
{% with btn_connect_text|default:_"Like" as btn_connect_text %}
{% with btn_cancel_text|default:_"Unlike" as btn_cancel_text %}
{% with predicate|default:"interest" as predicate %}
    {% if m.acl.user.id as user %}
        {# If the user is logged in, show its connect status #}

        {% if m.edge.id[user][predicate][id]|is_defined %}
            {# User has connected #}
            {% button
                text=btn_cancel_text
                class=btn_class++" is-active"
                action={
                    unlink
                    subject_id=user
                    predicate=predicate
                    object_id=id
                }
            %}
        {% elif can_connect %}
            {# User did not connect #}
            {% if predicate != 'rsvp' %}
                {% button
                    text=btn_connect_text
                    class=btn_class
                    action={
                        link
                        subject_id=user
                        predicate=predicate
                        object_id=id
                    }
                %}
            {% else %}
                <div class="with-elm">
                    <div class="list__item">
                        <div id="rsvp_tags_control"></div>
                    </div>
                </div>
                {% if not id|member:user.o.rsvp %}
                    {% with id.o.subject as subjects %}
                    {% javascript %}
                        var rsvpTagsControl = document.getElementById("rsvp_tags_control");
                        var rsvpTagsApp = Elm.Edit.Edge.init({
                            node: rsvpTagsControl,
                            flags: {
                                subject: {{ user }},
                                predicate: "subject",
                                category: "keyword",
                                checkboxes: true,
                                minimum: 1,
                                maximum: null,
                                allowNew: null,
                                label: "In welke van deze thema's ben je geïnteresseerd? (minimaal 1)",
                                placeholder: "Typ een letter om te zoeken",
                                suggestions: [
                                    {% for subject in subjects %}
                                    { id: {{subject.id}}, title: "{{subject.title}}" }
                                    {% if not forloop.last %}, {% endif %}
                                    {% endfor %}
                                ],
                                selection: [
                                    {% for subject in user.o.subject %}
                                    {% if subject|member:subjects %}
                                        { id: {{subject.id}}, title: "{{subject.title}}" }
                                        {% if not forloop.last %}, {% endif %}
                                    {% endif %}
                                    {% endfor %}
                                ],
                                textInputId: "rsvp_tags",
                                focus: false
                            }
                        });
                    {% endjavascript %}
                    {% endwith %}
                {% endif %}
                <p>
                    {% button
                        text=btn_connect_text
                        class=btn_class
                        postback={
                            link_rsvp
                            subject_id=user
                            predicate=predicate
                            object_id=id
                        }
                        delegate="kenniscloud"
                    %}
                </p>
            {% endif %}
        {% endif %}
    {% elif can_connect %}
        {# The user is not logged in. Show a dialog for login and connect. #}
        {% button
            text=btn_connect_text
            class=btn_class
            action={redirect dispatch="logon" p={page id=id}|url}
        %}
    {% endif %}

{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
