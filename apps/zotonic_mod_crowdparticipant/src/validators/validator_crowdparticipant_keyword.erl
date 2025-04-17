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

-module(validator_crowdparticipant_keyword).
-author("Driebit <tech@driebit.nl>").

-export([validate/5]).

-include("zotonic_core/include/zotonic.hrl").

%% @doc Validates one 'keyword' of a crowd participant.
%% Rejects keyword IDs that are unknown.
%%
%% Event handling is set up to handle this as a postback form validator, so to
%% check an input field with id 'keyword' it's sufficient to add to the template:
%% {% validate id="keyword" type={postback event="crowdparticipant_keyword"} %}
%%
%% Note: It now checks whether the keyword is of category `keyword` whereas
%%       it probably needs to be checking against `library_keyword` eventually.
%%       Library keywords are only the keywords imported from an external source;
%%       at the moment this is not an exhaustive list of keywords the library
%%       might want to use. (It is not even completely covering the domain of
%%       the external source.)
validate(crowdparticipant_keyword, Id, Keyword, _Args, Context) ->
    case m_rsc:is_a(Keyword, keyword, Context) of
        true -> {{ok, Keyword}, Context};
        false -> {{error, Id, invalid_keyword}, Context}
    end.
