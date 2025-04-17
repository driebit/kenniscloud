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

%% return all remarks creators unique

-module(filter_kc_get_remarkers).
-export([kc_get_remarkers/3]).
-include("zotonic_core/include/zotonic.hrl").

kc_get_remarkers(Id, Predicate, Context) ->
    case m_edge:subjects(Id, Predicate, Context) of
        [] ->
            [];
        Ids ->
            Creators = get_creators(Ids, Context),
            All = get_all_remarkers(Ids, Predicate, Creators, Context),
            lists:usort(All)
    end.

get_all_remarkers(Ids, Predicate, Creators, Context) when is_list(Ids) ->
    lists:foldl(fun(Id, List) ->
            ReplyIds = m_edge:subjects(Id, Predicate, Context),
            get_creators(ReplyIds, Context) ++ List
        end,
        Creators,
        Ids).

get_creators(Ids, Context) when is_list(Ids) ->
    lists:foldl(fun(Id, List) ->
            get_creator(Id, Context) ++ List
        end,
        [],
        Ids).

get_creator(Id, Context) ->
    [m_rsc:p(Id, creator_id, Context)].
