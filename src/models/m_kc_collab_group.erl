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
    includes_person/3,
    sql_user_collabs/2,
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
m_get([ CollabGroup, <<"includes_person">>, User | Rest], _Msg, Context) ->
    {ok, {includes_person(User, CollabGroup, Context), Rest}};
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
    CacheKey = {kc_roles_of, User, CollabGroup},
    case z_depcache:get(CacheKey, Context) of
        {ok, Roles} ->
            Roles;
        _ ->
            Roles = lists:filtermap(
                fun ({Role, Checked}) ->
                    if
                        Checked -> {true, Role};
                        true -> false
                    end
                end,
                checked_roles_of(User, CollabGroup, Context)
            ),
            Deps = [
                User,
                CollabGroup,
                m_rsc:rid(collection_expert_predicates, Context)
            ],
            z_depcache:set(CacheKey, Roles, ?DAY, Deps, Context),
            Roles
    end.

checked_roles_of(User, CollabGroup, Context) ->
    [
        {community_librarian,
            m_kc_user:is_community_librarian(User, Context)
        },
        {project_leader,
            kenniscloud_utils:edge_exists(CollabGroup, hascollabmanager, User, Context)
        },
        {manager,
            kenniscloud_utils:edge_exists(CollabGroup, hascollabmanager, User, Context)
            orelse kenniscloud_utils:edge_exists(CollabGroup, hasinitiator, User, Context)
        },
        {member,
            kenniscloud_utils:edge_exists(CollabGroup, hascollabmember, User, Context)
        }
    ] ++
    case m_edge:subjects(CollabGroup, has_subgroup, Context) of
        [] -> [];
        [Project | _] ->
            lists:map(
                fun (SpecialistPredicate) ->
                    {SpecialistPredicate,
                        kenniscloud_utils:edge_exists(Project, SpecialistPredicate, User, Context)
                    }
                end,
                m_edge:objects(collection_expert_predicates, haspart, Context)
            )
    end.

includes_person(Person, CollabGroup, Context) ->
    kenniscloud_utils:edge_exists(CollabGroup, hascollabmember, Person, Context) orelse
    kenniscloud_utils:edge_exists(CollabGroup, hascollabmanager, Person, Context) orelse
    kenniscloud_utils:edge_exists(CollabGroup, hasinitiator, Person, Context).

% SQL that returns all collab group IDs of which the user is a member
% IMPORTANT: for ACL this has to logically match 'includes_person' above
sql_user_collabs(UserId, Context) ->
    User = z_convert:to_list(UserId),
    "SELECT subject_id FROM edge " ++
    "WHERE predicate_id IN (" ++
        z_convert:to_list(m_rsc:rid(hascollabmember, Context)) ++ "," ++
        z_convert:to_list(m_rsc:rid(hascollabmanager, Context)) ++ "," ++
        z_convert:to_list(m_rsc:rid(hasinitiator, Context)) ++
    ") AND object_id = " ++ User.

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
