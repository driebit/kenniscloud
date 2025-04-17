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

-module(m_kc_user).
-author("Driebit <tech@driebit.nl>").

-export([
    m_get/3,

    knowledge_groups/2,
    recommended_knowledge_groups/2,
    regions/2,
    specialist_predicates_for/3,
    is_community_librarian/2,
    is_project_leader_of/3,
    roles_in/3
]).

-behaviour(zotonic_model).
-include("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> {ok, { term(), list() }} | {error, term()}.
m_get([<<"activity_inbox">>, <<"alert">> | Rest ] = _Path, _Msg, Context) ->
    User = z_acl:user(Context),
    Count = m_driebit_activity2_inbox:count_for_user(User, Context),
    Alert =
        case Count of
            0 -> false;
            _ ->
                MostRecentAt = m_driebit_activity2_inbox:most_recent_at_for_user(User, Context),
                LastSeenAt = m_driebit_activity2_inbox:seen_at_for_user(User, Context),
                case { LastSeenAt, MostRecentAt } of
                    { undefined, _ } -> true;
                    { _, undefined } -> true;
                    _ -> LastSeenAt < MostRecentAt
                end
        end,
    {ok, {Alert, Rest}};
m_get([<<"activity_inbox">> | Rest ] = _Path, _Msg, Context) ->
    {ok, {m_driebit_activity2_inbox:for_user(z_acl:user(Context), Context), Rest}};
m_get([<<"is_community_librarian">> | Rest ] = _Path, _Msg, Context) ->
    {ok, {is_community_librarian(z_acl:user(Context), Context), Rest}};
m_get([<<"is_project_leader_of">>, Project | Rest ] = _Path, _Msg, Context) ->
    {ok, {is_project_leader_of(Project, z_acl:user(Context), Context), Rest}};
m_get([<<"recommended_knowledge_groups">> | Rest ] = _Path, _Msg, Context) ->
    {ok, {recommended_knowledge_groups(z_acl:user(Context), Context), Rest}};

% used in list items for other users then the current user
m_get([ User, <<"roles_in">>, ProjectOrGroup | Rest ], _Msg, Context) ->
    {ok, {roles_in(ProjectOrGroup, User, Context), Rest}};
m_get([ User, <<"specialist_predicates_for">>, CollabGroup | Rest ], _Msg, Context) ->
    {ok, {specialist_predicates_for(User, CollabGroup, Context), Rest}};

% Unexpected path
m_get(_, _Msg, _Context) ->
    {ok, {undefined, []}}.

is_community_librarian(UserId, Context) ->
    CommunityLibrarian = m_rsc:name_lookup(acl_user_group_community_librarian, Context),
    lists:member(CommunityLibrarian, m_edge:objects(UserId, hasusergroup, Context)).

is_project_leader_of(GroupId, UserId, Context) ->
    m_edge:get_id(GroupId, hascollabmanager, UserId, Context) =/= undefined.

roles_in(GroupId, UserId, Context) ->
    case m_rsc:is_a(GroupId, acl_collaboration_group, Context) of
        true ->
            case is_project_leader_of(GroupId, UserId, Context) of
                true ->
                    [project_leader];
                false ->
                    []
            end;
        false ->
            m_kc_collab_group:roles_of(UserId, GroupId, Context)
    end.

knowledge_groups(UserId, Context) ->
    m_rsc:s(UserId, hascollabmember, Context).


% Returns a list of (up to) 20 recommended knowledge groups, based on the ones
% that the user is already a member of (if any).
recommended_knowledge_groups(UserId, Context) ->
    #search_result{ result = Result } = z_search:search(
        <<"query">>,
        #{
            <<"match_many_objects">> => knowledge_groups(UserId, Context),
            <<"cat_exact">> => acl_collaboration_group
        },
        1,
        20,
        Context
    ),
    Result.

regions(UserId, Context) ->
    m_rsc:o(UserId, hasregion, Context).

specialist_predicates_for(UserId, Project, Context) ->
    SpecialistPredicates = m_edge:objects(collection_expert_predicates, haspart, Context),
    lists:filter(
        fun (Pred) -> m_edge:get_id(Project, Pred, UserId, Context) =/= undefined end,
        SpecialistPredicates
    ).
