
{# this template was import from the zotonic_mod_driebit_base module to simplify and override some of the default styling and html  #}
{% with
    formclass|default:"search-suggestions__searchform",
    togglebutton|default:"toggle-search",
    wrapperclass|default:"search-suggestions__searchform__group",
    buttonclass|default:"search-suggestions__submit",
    suggestionsclass|default:"search-suggestions__suggestions",
    placeholder|default:_"Search",
    buttonlabel|if_undefined:_"Search",
    auto_disable|default:false,
    iconclass
as
    formclass,
    togglebutton,
    wrapperclass,
    buttonclass,
    suggestionsclass,
    placeholder,
    buttonlabel,
    auto_disable,
    iconclass
%}

<form class="{{ formclass }} {{ extraFormClassess }}" id="search-suggestions__searchform-{{ #identifier }}" role="search" action="{% if context %}/{{ context }}_search{% else %}{% url search %}{% endif %}" method="get">
    <div class="{{ wrapperclass }}">
        <label style="display:none;" class="search-form__label" for="qs">{_ Search _}</label>
        <div class="c-nav-search__input">
          <button type="submit" class="{{ buttonclass }}" title="{_ Search _}"><img src="/lib/images/icon-font/--search.svg" /></button>

          <input type="search"
              aria-label=_"Search"
              class="do_search_suggestions"
              name="qs"
              id="qs"
              value="{{q.qs|escape}}"
              placeholder="{{ placeholder }}"
              autocomplete="off"
              data-param-wire="show-suggestions-{{ #identifier }}"
              data-param-results="search-suggestions__suggestions-{{ #identifier }}"
              data-param-togglebutton="{{ togglebutton }}"
              data-param-auto-disable="{{ auto_disable }}"
            />
          </div>
          

          {% block search_suggestions_wire %}
            {% wire name="show-suggestions-"++#identifier
                action={update target="search-suggestions__suggestions-"++#identifier template="search-suggestions/search-query-wrapper.tpl" pagelen=12 results_template="search-suggestions/search-suggestions.tpl" context=context  }
            %}
          {% endblock %}
        <div id="suggestion-container" style="display: none;">
          <div class="{{ suggestionsclass }}" id="search-suggestions__suggestions-{{ #identifier }}"></div>
        </div>
    </div>
</form>
{% endwith %}

{% javascript %}
var searchSuggestionsContainer = document.getElementById("suggestion-container");
var searchInput = document.getElementById("qs");

if (!searchSuggestionsContainer && !searchInput) {
    return; 
} else {
  searchInput.addEventListener("keyup", function() {
      console.log("trigger");
      if (!searchInput.value) {
          searchSuggestionsContainer.style.display = "none";
          console.log("hide suggestions");
      } else {
          searchSuggestionsContainer.style.display = "block";
      }
  });
}

{% endjavascript %}
