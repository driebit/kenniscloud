{% if id.has_opnform and id.has_opnformlink %}
    <div class="c-external-services__opnform">
    <iframe style="border:none;width:100%; height: 100%;" id="{{ id.has_opnformlink }}" src="https://opnform.com/forms/{{ id.has_opnformlink }}"></iframe><script type="text/javascript" onload="initEmbed('{{ id.has_opnformlink }}')" src="https://opnform.com/widgets/iframe.min.js"></script>
    </div>
{% endif %}