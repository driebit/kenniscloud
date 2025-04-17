{% wire id=#form type="submit" postback={new_collabgroup} delegate=`kenniscloud_collabgroup_tools` %}
<form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">
    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #name }}">Titel</label>
        <div class="col-md-6">
	        <input type="text" id="{{ #name }}" name="name" value="" class="form-control" tabindex="1" autofocus />
	        {% validate id=#name name="name" type={presence} %}
	    </div>
    </div>

    <div class="modal-footer">
      {% button class="btn btn-default" action={dialog_close} text=_"Cancel" %}
      <button class="btn btn-primary" type="submit">Voeg kennisgroep toe</button>
    </div>

</form>
