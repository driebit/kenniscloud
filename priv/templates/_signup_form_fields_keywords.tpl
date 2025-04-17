<div class="form-group with-elm" id="signup_tags_group">
    <label for="signup_tags" class="control-label">In welke thema's ben je geïnteresseerd?</label>
    <input id="signup_tags_validated" hidden=true value="1">
    <div id="signup_tags_control"></div>

    {# Note: this validator is defined in "_js_include.tpl" #}
    {% validate id="signup_tags_validated" type={custom failureMessage="Vul minimaal 1 interesse in" against="window.validateSignupTags"} %}
</div>
