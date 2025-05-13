<div class="form-group">
    <div>
        {% button class="btn btn-default" text=_"Fetch AZB keywords"
            postback=`update_keywords` delegate=`kenniscloud_azb`
            action={script script="queueCountInfo('#pivot-queue-count')"}
        %}
        <p class="help-block">
            {_ Triggers a refetch of the library keywords from the AZB API. _}
        </p>
    </div>
</div>
