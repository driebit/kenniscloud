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
    m_post/3,
    m_delete/3,

    knowledge_groups/2,
    recommended_knowledge_groups/2,
    regions/2,
    specialist_predicates_for/3,
    is_community_librarian/2,
    is_project_leader_of/3
]).

-behaviour(zotonic_model).
-include("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> {ok, { term(), list() }} | {error, term()}.
m_get([<<"activity_inbox">>, <<"alert">> | Rest ] = _Path, _Msg, Context) ->
    User = z_acl:user(Context),
    Alert =
        case m_activity:inbox_activities(User, Context) of
            [] -> false;
            [LatestActivity | _] ->
                MostRecentAt = m_rsc:p_no_acl(LatestActivity, <<"modified">>, Context),
                LastSeenAt = kenniscloud_activity:notifications_seen_at(User, Context),
                case { LastSeenAt, MostRecentAt } of
                    { undefined, _ } -> true;
                    { _, undefined } -> true;
                    _ -> LastSeenAt < MostRecentAt
                end
        end,
    {ok, {Alert, Rest}};
m_get([<<"is_community_librarian">> | Rest ] = _Path, _Msg, Context) ->
    {ok, {is_community_librarian(z_acl:user(Context), Context), Rest}};
m_get([<<"is_project_leader_of">>, Project | Rest ] = _Path, _Msg, Context) ->
    {ok, {is_project_leader_of(Project, z_acl:user(Context), Context), Rest}};
m_get([<<"recommended_knowledge_groups">> | Rest ] = _Path, _Msg, Context) ->
    {ok, {recommended_knowledge_groups(z_acl:user(Context), Context), Rest}};

% used in list items for other users then the current user
m_get([ User, <<"specialist_predicates_for">>, CollabGroup | Rest ], _Msg, Context) ->
    {ok, {specialist_predicates_for(User, CollabGroup, Context), Rest}};
m_get([ User, <<"is_community_librarian">> | Rest ] = _Path, _Msg, Context) ->
    {ok, {is_community_librarian(User, Context), Rest}};

% Unexpected path
m_get(_, _Msg, _Context) ->
    {ok, {undefined, []}}.

-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) ->
    {ok, term()} | ok | {error, term()}.
% API path: /api/model/kc_user/post/activity_inbox
m_post([ <<"activity_inbox">> | _Rest ], _Msg, Context) ->
    kenniscloud_activity:seen_notifications(Context),
    ok;
% Unexpected query
m_post(_Path, _Payload, _Context) ->
    {error, unknown_path}.


-spec m_delete( list(), zotonic_model:opt_msg(), z:context() ) ->
    {ok, term()} | ok | {error, term()}.
% API path: /api/model/kc_user/delete/activity_inbox
m_delete([ <<"activity_inbox">> | _Rest ], _Msg, Context) ->
    m_activity:clear_inbox(Context);
% Unexpected path
m_delete(_Path, _Msg, _Context) ->
    {error, unknown_path}.


is_community_librarian(UserId, Context) ->
    CommunityLibrarian = m_rsc:name_lookup(acl_user_group_community_librarian, Context),
    lists:member(CommunityLibrarian, m_edge:objects(UserId, hasusergroup, Context)).

is_project_leader_of(GroupId, UserId, Context) ->
    m_edge:get_id(GroupId, hascollabmanager, UserId, Context) =/= undefined.

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
        3,
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
