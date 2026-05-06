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

%% @author Driebit <tech@driebit.nl>
%% @copyright 2017

-module(m_kc_region).
-author("Driebit <tech@driebit.nl>").

-export([
    m_get/3,
    get_community_librarians/2,
    get_knowledge_groups/2
]).

-behaviour(zotonic_model).
-include("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> {ok, { term(), list() }} | {error, term()}.
% Syntax: m.kc_region[RegionId].community_librarians
m_get([ Region, <<"community_librarians">> | Rest ], _Msg, Context) ->
    case z_acl:user(Context) of
        undefined ->
            {ok, {undefined, []}};
        _ ->
            {ok, {get_community_librarians(Region, Context), Rest}}
    end;
% Syntax: m.kc_region[RegionId].knowledge_groups
m_get([ Region, <<"knowledge_groups">> | Rest ], _Msg, Context) ->
    {ok, {get_knowledge_groups(Region, Context), Rest}};
m_get(_, _Msg, _Context) ->
    {ok, {undefined, []}}.


%% @doc Get all community librarians for a region.
%% Person are a region's community librarians if:
%% - they are manager of a Knowledge group in the region
%% - and they are member of the user group Community Librarian
-spec get_community_librarians(m_rsc:resource(), z:context()) -> #search_result{}.
get_community_librarians(RegionId, Context) ->
    CacheKey = {kc_community_librarians, RegionId},
    case z_depcache:get(CacheKey, Context) of
        {ok, CommunityLibrarians} ->
            CommunityLibrarians;
        _ ->
            #search_result{ result = CommunityLibrarians } = z_search:search(
                <<"query">>,
                [
                    {region_kg_manager, RegionId},
                    {hasobject, [acl_user_group_community_librarian, hasusergroup]},
                    {sort, "-pivot.kenniscloud_users.has_depiction"}
                ],
                1, 10000,
                Context
            ),
            Deps = [RegionId, m_rsc:rid(acl_user_group_community_librarian, Context)],
            z_depcache:set(CacheKey, CommunityLibrarians, ?DAY, Deps, Context),
            CommunityLibrarians
    end.

get_knowledge_groups(RegionId, Context) ->
    CacheKey = {region_knowledge_groups, RegionId},
    case z_depcache:get(CacheKey, Context) of
        {ok, KnowledgeGroups} ->
            KnowledgeGroups;
        _ ->
            #search_result{ result = KnowledgeGroups } = z_search:search(
                <<"query">>,
                #{
                    <<"cat">> => acl_collaboration_group,
                    <<"hasobject">> => [RegionId, hasregion]
                },
                1,
                10000,
                Context
            ),
            z_depcache:set(CacheKey, KnowledgeGroups, ?DAY, [RegionId | KnowledgeGroups], Context),
            KnowledgeGroups
    end.
