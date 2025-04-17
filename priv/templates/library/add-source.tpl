{% if id.is_editable %}
    <form id="add-reference" class="add-source" method="post" action="postback">
        <label for="uri-input" class="bordered-title"><span>Voeg een URL toe:</span></label>

        <div class="add-source__input">
            <input id="uri-input" type="text" name="uri" value="" />
            <button id="uri-submit" class="btn--dark" type="submit" disabled>Toevoegen</button>
        </div>

        <small>Bijvoorbeeld uit de <a target="_blank" href="https://www.bibliotheek.nl">collectie van de bibliotheek</a></small>
    </form>

    {% wire
        id="add-reference"
        type="submit"
        postback={
            add_reference
            subject_id=id
            target_id="add-reference"
        }
        delegate=`kenniscloud_reference`
    %}

    {% javascript %}
        document.querySelector('#uri-input').addEventListener('input', function(event) {
            var is_uri_input_empty = event.target.value.trim() === '';
            document.querySelector('#uri-submit').disabled = is_uri_input_empty
        });
    {% endjavascript %}
{% endif %}
