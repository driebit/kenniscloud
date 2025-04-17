<p>
    Ik wil een nieuwe kennisgroep aanmaken.
</p>

<div class="tab-content" id="dialog-connect-panels">

    {% wire id=#form type="submit"
    	postback={
            new_group
            subject_id=id.id
            objects=[[m.acl.user, 'hascollabmanager']]
            predicate="haspart"
            redirect="ginger_edit"
            actions=[]
            callback=""
        }
    	delegate=`kenniscloud`
    %}

    <form id="{{ #form }}" method="POST" action="postback" class="form">

    	<input type="hidden" name="category_id" value="{{ m.rsc.acl_collaboration_group.id }}"/>
        <input type="hidden" name="content_group_id" value="own"/>
        <input type="hidden" name="is_published" value="1"/>

    	<div class="form-group row">
    	    <label class="control-label col-md-3" for="new_rsc_title">{_ Title _}</label>
    	    <div class="col-md-9">
    		<input type="text" id="new_rsc_title" name="new_rsc_title" value="" class="do_autofocus form-control" />
    		{% validate id="new_rsc_title" type={presence} %}
    	    </div>
    	</div>

        <div class="modal-footer">
        	{% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        	<button class="btn btn-primary" type="submit">{_ Make _} {{ catname }}</button>
        </div>

    </form>

</div>
