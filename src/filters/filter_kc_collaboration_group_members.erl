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

-module(filter_kc_collaboration_group_members).

-export([kc_collaboration_group_members/2]).

-include("zotonic_core/include/zotonic.hrl").

kc_collaboration_group_members(CollabGroup, Context) ->
    CacheKey = {kc_collaboration_group_members, CollabGroup},
    case z_depcache:get(CacheKey, Context) of
        {ok, AllMembers} ->
            AllMembers;
        _ ->
            #search_result{ result = Managers } = z_search:search(
                <<"query">>,
                [
                    {hassubject, [CollabGroup, hascollabmanager]},
                    {sort, "-pivot.kenniscloud_users.has_depiction"}
                ],
                1, 10000,
                Context
            ),
            #search_result{ result = Members } = z_search:search(
                <<"query">>,
                [
                    {hassubject, [CollabGroup, hascollabmember]},
                    {sort, "-pivot.kenniscloud_users.has_depiction"}
                ],
                1, 10000,
                Context
            ),
            AllMembers = lists:uniq(Managers ++ Members),
            z_depcache:set(CacheKey, AllMembers, ?DAY, [CollabGroup | AllMembers], Context),
            AllMembers
    end.
