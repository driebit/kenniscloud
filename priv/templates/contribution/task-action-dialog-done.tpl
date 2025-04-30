<p><strong>{_ Warning! _}</strong> {_ This can not be undone. _}</p>
<div class="modal-footer">
    {% button class="btn btn--cancel" text=_"Cancel" action={dialog_close} %}
    {% button
        text=_"Mark task as done"
        class="btn btn--primary"
        postback={complete_task id=id dispatch_to=dispatch_to|default:'page'}
        delegate=`kenniscloud`
    %}
</div>
