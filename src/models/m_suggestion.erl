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

-module(m_suggestion).

-export([
    m_get/3
]).

-behaviour(zotonic_model).
-include("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> {ok, { term(), list() }} | {error, term()}.

% For API access via 'controller_api'.
% Path: /api/model/suggestion/get/<search_parameters>
m_get(Path, Msg, Context) ->
    % We defer the search itself to 'm_search' and in case of positive result we
    % convert each resulting ID in a map with 'id' and 'title' (see 'encodeSuggestion').
    case m_search:m_get(Path, Msg, Context) of
        {ok, #search_result{ result = Result }} when is_list(Result) ->
            {ok, encodeSuggestions(Result, Context)};
        {ok, {#search_result{ result = Result }, _Rest}} when is_list(Result) ->
            {ok, encodeSuggestions(Result, Context)};
        Other ->
            Other
    end.


encodeSuggestions(List, Context) ->
    lists:map(fun(Elem) -> encodeSuggestion(Elem, Context) end, List).

encodeSuggestion(SuggestionId, Context) ->
    #{
        id => SuggestionId,
        title => kenniscloud_utils:ensure_trans(m_rsc:p(SuggestionId, title, Context))
    }.
