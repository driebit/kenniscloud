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

kc_collaboration_group_progress_label(Ids, Context) when is_list(Ids) ->
    case lists:dropwhile(
        fun(Id) ->
            case m_rsc:get(Id, Context) of
                Rsc when is_map(Rsc) ->
                    ProgressLabel = maps:get(<<"progress_label">>, Rsc, undefined),
                    CategoryId = maps:get(<<"category_id">>, Rsc, undefined),
                    IsEvent = case m_category:id_to_name(CategoryId, Context) of
                        event ->
                            true;
                        _ ->
                            false
                    end,
                    case {ProgressLabel, IsEvent} of
                        {undefined, false} -> true;
                        {<<>>, false} -> true;
                        _ -> false
                    end;
                _ ->
                    true
            end
        end,
        Ids
    ) of
        [FirstId | _] -> m_rsc:get(FirstId, Context);
        [] -> undefined
    end;
kc_collaboration_group_progress_label(_, _) ->
    undefined.


