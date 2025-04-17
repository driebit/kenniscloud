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

-module(m_kc_collab_group).
-author("Driebit <tech@driebit.nl>").

-export([
    m_get/3,
    collab_group_of/2,
    roles_of/3,
    project_leaders/2,
    managers/2,
    specialists/2,
    members/2,
    private_acl_rule_id/2,
    private_acl_rule_id/3,
    private_acl_rule/2
]).

-behaviour(zotonic_model).
-include("zotonic_core/include/zotonic.hrl").

-type role()
    :: community_librarian
     | project_leader
     | manager
     | member
     | m_rsc:resource_id().

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> {ok, { term(), list() }} | {error, term()}.
m_get([ CollabGroup, <<"roles_of">>, User | Rest ], _Msg, Context) ->
    {ok, {roles_of(User, CollabGroup, Context), Rest}};
m_get([ CollabGroup, <<"private_acl_rule_id">> | Rest ], _Msg, Context) ->
    {ok, {private_acl_rule_id(CollabGroup, Context), Rest}};
m_get([ <<"collab_group_of">>, Rsc | Rest ], _Msg, Context) ->
    {ok, {collab_group_of(Rsc, Context), Rest}};
m_get(_, _Msg, _Context) ->
    {ok, {undefined, []}}.

collab_group_of(Id, Context) ->
    ContentGroup = m_rsc:p_no_acl(Id, content_group_id, Context),
    case m_rsc:is_a(ContentGroup, acl_collaboration_group, Context) of
        true ->
            ContentGroup;
        false ->
            undefined
    end.

% Returns a list with an atom for the role, or the id of the relevant predicate in case of a specilist
-spec roles_of(m_rsc:resource_id(), m_rsc:resource_id(), #context{}) -> [role()].
roles_of(User, CollabGroup, Context) ->
    MemberGroups = [
        {project_leader, project_leaders(CollabGroup, Context)},
        {manager, managers(CollabGroup, Context)}
    ] ++ specialists(CollabGroup, Context) ++
    [
        {member, members(CollabGroup, Context)}
    ],

    Roles = lists:filtermap(
        fun ({Role, MemberGroup}) ->
            case lists:member(User, MemberGroup) of
                true -> {true, Role};
                false -> false
            end
        end,
        MemberGroups
    ),

    case m_kc_user:is_community_librarian(User, Context) of
        true -> [community_librarian|Roles];
        false -> Roles
    end.

project_leaders(CollabGroup, Context) ->
    m_edge:objects(CollabGroup, hascollabmanager, Context).

managers(CollabGroup, Context) ->
    m_edge:objects(CollabGroup, hascollabmanager, Context)
        ++ m_edge:objects(CollabGroup, hasinitiator, Context).

specialists(CollabGroup, Context) ->
    case m_edge:subjects(CollabGroup, has_subgroup, Context) of
        [] -> [];
        [Project | _] ->
            SpecialistPredicates = m_edge:objects(collection_expert_predicates, haspart, Context),
            lists:map(fun (Pred) -> {Pred, m_edge:objects(Project, Pred, Context)} end, SpecialistPredicates)
    end.

members(CollabGroup, Context) ->
    #search_result{ result = Members } = z_search:search(
        <<"query">>,
        [
            {hassubject, [CollabGroup, hascollabmember]},
            {sort, "-pivot.kenniscloud_users.has_depiction"}
        ],
        1, 10000,
        Context
    ),
    Members.

private_acl_rule_id(CollabGroup, Context) ->
    private_acl_rule_id(CollabGroup, acl_rules_is_edit_state(Context), Context).

private_acl_rule_id(CollabGroup, IsEdit, Context) ->
    {Query, Params} = private_acl_rule_constant_props_query(Context),
    ParamsLength = length(Params),
    Query1 =
        lists:flatten([
            Query,
            " and content_group_id = $", integer_to_list(ParamsLength + 1),
            " and is_edit = $", integer_to_list(ParamsLength + 2)
        ]),
    Params1 = Params ++ [CollabGroup, IsEdit],
    case z_db:assoc_row(Query1, Params1, Context) of
        [{id, Id}] -> Id;
        _ -> undefined
    end.

private_acl_rule_constant_props_query(Context) ->
    Rule = private_acl_rule_constant_props(Context),
    Query =
        "select id from acl_rule_rsc
            where is_block = $1
            and actions = $2
            and acl_user_group_id = $3
            and is_owner = $4
            and category_id is null
            and managed_by = $5",
    Params = lists:map(fun ({_Key, Value}) -> Value end, proplists:delete(category_id, Rule)),
    {Query, Params}.

private_acl_rule(CollabGroup, Context) ->
    [{content_group_id, CollabGroup}|private_acl_rule_constant_props(Context)].

% Be careful when updating this proplist. Any existing ACL deny rules for private collab groups may not match anymore.
% If they don't they need to be recreated.
private_acl_rule_constant_props(Context) ->
    AnonUserGroup = m_rsc:rid(acl_user_group_anonymous, Context),
    [
        {is_block,true},
        {actions,<<"link,delete,update,insert,view">>},
        {acl_user_group_id,AnonUserGroup},
        {is_owner,false},
        {category_id,undefined},
        {managed_by, <<"kenniscloud_private_collab_groups">>} % Non-existent module prevents reset on manage_schema
    ].

acl_rules_is_edit_state(Context) ->
    case acl_user_groups_checks:session_state(Context) of
        publish -> false;
        edit -> true;
        _ -> false
    end.
