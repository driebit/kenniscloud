%% @author Driebit <tech@driebit.nl>
%% @copyright 2025 Driebit

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

%% @doc Filter to return the first item in a list with a progress_label or if that item is an event.
-module(filter_kc_collaboration_group_progress_label).
-export([kc_collaboration_group_progress_label/2]).

-include("zotonic_core/include/zotonic.hrl").

kc_collaboration_group_progress_label(GroupId, Context) when is_integer(GroupId) ->
    #search_result{ result = RscIds } = z_search:search(
        <<"query">>,
        [
            {<<"cat">>, [<<"contribution">>, <<"event">>]},
            {<<"is_published">>, true},
            {<<"content_group">>, GroupId},
            {<<"sort">>, <<"-rsc.created">>}
        ],
        1,
        15, % pagelen
        Context
    ),

    ProgressResult = lists:search(
        fun(RscId) ->
            case m_rsc:is_a(RscId, event, Context) of
                true ->
                    true;
                false ->
                    case m_rsc:p(RscId, <<"progress_label">>, Context) of
                        undefined -> false;
                        <<>> -> false;
                        _ -> true
                    end
            end
        end,
        RscIds
    ),

    case ProgressResult of
        false -> undefined;
        {value, Value} -> Value
    end;
kc_collaboration_group_progress_label(_, _) ->
    undefined.


