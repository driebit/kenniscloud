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
    get_community_librarians/2
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
m_get(_, _Msg, _Context) ->
    {ok, {undefined, []}}.


%% @doc Get all community librarians for a region.
%% Person are a region's community librarians if:
%% - they are manager of a Knowledge group in the region
%% - and they are member of the user group Community Librarian
-spec get_community_librarians(m_rsc:resource(), z:context()) -> #search_result{}.
get_community_librarians(RegionId, Context) ->
    KnowledgeGroups = get_knowledge_groups(RegionId, Context),
    RegionManagers = lists:foldl(
        fun(KnowledgeGroup, KgManagers) ->
            KgManagers ++ m_rsc:o(KnowledgeGroup, hascollabmanager, Context)
        end,
        [],
        KnowledgeGroups
        ),
    {ok, ClUserGroup} = m_rsc:name_to_id(acl_user_group_community_librarian, Context),
    lists:filter(
        fun (User) ->
            UserGroups = m_rsc:o(User, hasusergroup, Context),
            lists:member(ClUserGroup, UserGroups)
        end,
        lists:usort(RegionManagers)
    ).

get_knowledge_groups(RegionId, Context) ->
    Result = m_search:search({query, [
        {cat, acl_collaboration_group},
        {hasobject, [RegionId, hasregion]}
    ]}, Context),

    filter_make_list:make_list(Result, Context).
