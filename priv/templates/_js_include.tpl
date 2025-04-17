
{% include "_js_include_jquery.tpl" %}

{% lib
    "js/modules/jstz.min.js"
    "cotonic/cotonic.js"
    "js/apps/zotonic-wired.js"
    "js/apps/z.widgetmanager.js"
    "js/modules/z.notice.js"
    "js/modules/z.inputoverlay.js"
    "js/modules/z.imageviewer.js"
    "js/modules/z.dialog.js"
    "js/modules/z.popupwindow.js"
    "js/modules/z.live.js"
    "js/modules/z.feedback.js"
    "js/modules/livevalidation-1.3.js"
    "js/modules/jquery.loadmask.js"
    "js/global-nav.js"
    "js/search-suggestions.js"
    "bootstrap/js/bootstrap.min.js"
    "js/touch-hover.js"
%}


{# Validator for "_signup_form_fields_keywords.tpl". This is defined here because
# we want the validation function to be accessible as 'window.validateSignupTags'
# and not to be included in the computed JS script tag.
#}
{% if zotonic_dispatch == `signup` or zotonic_dispatch == `signup_confirm` %}
    <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
        var signupTagsControl = document.getElementById("signup_tags_control");
        var signupTagsApp = Elm.AddEdgeToNewResource.init({
            node: signupTagsControl,
            flags: {
                predicate: "subject",
                category: "keyword",
                placeholder: "Typ een letter om te zoeken",
                id: "signup_tags"
            }
        });

        var signupTagsValidated = false;
        signupTagsApp.ports.portValues.subscribe(
            function(values) {
                signupTagsValidated = values.validated;
            }
        );

        var validateSignupTags = function (value, args, isSubmit, submitTrigger) {
            if (signupTagsValidated)
                document.getElementById("signup_tags_group").classList.remove("has-error")
            else
                document.getElementById("signup_tags_group").classList.add("has-error");
            return signupTagsValidated;
        }
    </script>
{% endif %}

<script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
    $(function() { $.widgetManager(); });
</script>
