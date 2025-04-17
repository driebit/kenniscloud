%% @author Driebit <tech@driebit.nl>
%% @copyright 2025 Driebit

%% Copyright 2025 Driebit
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(validator_crowdparticipant_custom_keyword).
-author("Driebit <tech@driebit.nl>").

-export([validate/5]).

-include("zotonic_core/include/zotonic.hrl").

%% @doc Validates one 'custom_keyword' of a crowd participant.
%% Rejects keywords that are not between 1 and 30 chars in length and those that
%% contain characters different from letters and spaces.
%%
%% Event handling is set up to handle this as a postback form validator, so to
%% check an input field with id 'keyword' it's sufficient to add to the template:
%% {% validate id="keyword" type={postback event="crowdparticipant_custom_keyword"} %}
validate(crowdparticipant_custom_keyword, Id, Keyword, _Args, Context) ->
    case is_accepted_string(Keyword) of
        false -> {{error, Id, invalid_keyword_chars}, Context};
        true ->
            Min = 1,
            Max = 30,
            validator_base_length:validate(length, Id, Keyword, [Min,Max], Context)
    end.

is_accepted_string(Keyword) when is_list(Keyword) ->
    lists:all(fun is_accepted_char/1, Keyword);
is_accepted_string(Keyword) ->
    is_accepted_string(z_convert:to_list(Keyword)).

is_accepted_char(Char) when Char >= $a, Char =< $z -> true;
is_accepted_char(Char) when Char >= $A, Char =< $Z -> true;
is_accepted_char(Char) when Char >= $0, Char =< $9 -> true;
is_accepted_char(Char) when Char == $\s -> true;
is_accepted_char(_Char) -> false.
