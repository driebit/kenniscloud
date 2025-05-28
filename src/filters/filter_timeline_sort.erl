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

%% @doc filter to sort the items in the timeline for a kennisgroep
-module(filter_timeline_sort).

-export([timeline_sort/3]).

-include("zotonic_core/include/zotonic.hrl").

-spec timeline_sort(list(), m_rsc:resource_id(), z:context()) -> list().
timeline_sort(Items, RscId, Context) when is_list(Items) ->
    % First find the sorting date for each resource:
    IdsWithDates = lists:map(fun(Item) -> {Item, timeline_date(Item, Context)} end, Items),
    % then sort them on said date:
    {SortedItems, _} = lists:unzip(lists:keysort(2, IdsWithDates)),
    % finally reverse the list if needed:
    case z_convert:to_atom(m_rsc:p(RscId, <<"timeline_order">>, Context)) of
        desc -> lists:reverse(SortedItems);
        _ -> SortedItems
    end;
timeline_sort(#search_result{result=Result}, RscId, Context) ->
    timeline_sort(Result, RscId, Context).


% Fetch the date to be used for sorting from an RSC.
timeline_date(RscId, Context) ->
    case m_rsc:is_a(RscId, event, Context) of
        true ->
            % For meetups, use the start date if available
            case m_rsc:p(RscId, <<"date_start">>, Context) of
                undefined ->
                    % otherwise, use the publication date
                    m_rsc:p(RscId, <<"publication_start">>, Context);
                PublicationStart ->
                    PublicationStart
            end;
        false ->
            % For everything else, just use the publication date
            m_rsc:p(RscId, <<"publication_start">>, Context)
    end.
