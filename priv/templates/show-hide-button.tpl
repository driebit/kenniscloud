<button class="c-toggle-more" id={{ button_id }}>
    <span class="c-toggle-more__label-show">{_ Show more _}</span>
    <span class="c-toggle-more__label-hide">{_ Show less _}</span>
</button>
{% wire
    id=button_id
    action={toggle_class target=button_id class="c-toggle-more--open"} 
%}