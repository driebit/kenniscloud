<p>
    Weet je het zeker?
</p>

<div class="tab-content" id="dialog-connect-panels">

    {% wire id=#form type="submit"
    	postback={
            sudo_delete_profile
            redirect=redirect
        }
    	delegate=`kenniscloud`
    %}

    <form id="{{ #form }}" method="POST" action="postback" class="form">

        <div class="modal-footer">
        	{% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        	<button class="btn btn-primary" type="submit">Verwijder mijn profiel</button>
        </div>

    </form>

</div>
