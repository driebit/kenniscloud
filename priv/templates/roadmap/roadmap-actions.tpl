<div class="roadmap__actions">
    <a href="javascript:window.print()" class="btn--print"><i class="icon--pdf"></i> / <i class="icon--print"></i></a>
    {% if id.is_linkable %}
        {% button tag="a" text="Volgorde resetten" class="btn btn--primary" postback={resetorder id=id} delegate="kenniscloud_action_resetorder" %}
        <a href="{% url ginger_edit id=id %}" class="btn btn--primary">Volgorde bewerken</a>
    {% endif %}
</div>
