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

-module(kenniscloud_acl).

-export([
    is_allowed/2,
    is_allowed_prop/3,

    rules/0
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% Event/notification handlers

is_allowed(Query, Context) ->
    case is_allowed_explained(Query, Context) of
        undefined -> undefined;
        {Reason, Verdict} ->
            log_if_enabled("is_allowed ~p verdict ~p, by rule ~p", [Query, Verdict, Reason], Context),
            Verdict
    end.

is_allowed_prop(Id, Prop, Context) ->
    case is_allowed_prop_explained(Id, Prop, Context) of
        undefined -> undefined;
        {Reason, Verdict} ->
            log_if_enabled("is_allowed_prop ~p for ~p verdict ~p, by rule ~p", [Prop, Id, Verdict, Reason], Context),
            Verdict
    end.

%% Internal functions

log_if_enabled(Text, Args, Context) ->
    case z_convert:to_bool(m_config:get_value(kenniscloud, log_acl, false, Context)) of
        true -> ?zInfo(Text, Args, Context);
        _ -> ok
    end.

%% @doc ACL logic for Kenniscloud.
%%
%% Limits edit rights on collab groups/texts for project managers to the projects/collab groups they are project manager of
%% Allow users to link themselves to collab groups
%%
%% For rsc check, object is id, unless action is insert. Then it's acl_rsc
%% For edge check, object is acl_edge
%%
%% ACL rules defined in the ACL table can only be limited further, not expanded upon
-spec is_allowed_explained(#acl_is_allowed{}, z:context()) ->
    undefined | {Reason, Verdict} when
        Reason :: [string()],
        Verdict :: true | false | undefined.
is_allowed_explained(Query, Context) ->
    UserGroups = lists:map(fun (UserGroup) -> m_rsc:p_no_acl(UserGroup, name, Context) end, acl_user_groups_checks:user_groups_all(Context)),
    is_allowed_explained(UserGroups, Query, Context).


%% @doc part of 'is_allowed_explained/2' taking user groups into account.
-spec is_allowed_explained(list(UserGroupName), #acl_is_allowed_prop{}, z:context()) ->
    undefined | {Reason, Verdict} when
        UserGroupName :: binary(),
        Reason :: [string()],
        Verdict :: true | false | undefined.
% Managers and editors are not restricted here, but still limited by the table of ACL rules
is_allowed_explained([<<"acl_user_group_managers">> | _UGs], _Query, _Context) ->
    undefined;
is_allowed_explained([<<"acl_user_group_editors">> | _UGs], _Query, _Context) ->
    undefined;
% Anonymous visitors are not allowed to view private 'acl_collaboration_group'/kennisgroepen.
% This clause may seem redundant because there are "private rules" set up for
% these 'acl_collaboration_group' (see 'm_kc_collab_group'), however we need this
% as the general rule that anonymous users can see anything in 'acl_collaboration_group'
% (see 'rules/0' below) takes precedence.
% We could avoid using rules for private 'acl_collaboration_group' entirely and
% only handle this permission here, but we keep the current mechanism in order
% to be able to see the active rules in the CMS.
is_allowed_explained(
    [<<"acl_user_group_anonymous">> | _UGs],
    #acl_is_allowed{
        action = view,
        object = Rsc
    },
    Context
) when is_integer(Rsc) ->
    ContentGroupId = m_rsc:p_no_acl(Rsc, <<"content_group_id">>, Context),
    case m_rsc:is_a(ContentGroupId, acl_collaboration_group, Context) of
        true ->
            case m_kc_collab_group:private_acl_rule_id(ContentGroupId, Context) of
                RuleId when is_integer(RuleId) ->
                    {"forbid Anonymous from accessing private kennisgroepen", false};
                undefined ->
                    undefined
            end;
        _ ->
            undefined
    end;
% Members can insert a 'relation' edge between any 'contribution' and 'reference'
is_allowed_explained(
    [<<"acl_user_group_members">> | UserGroups],
    Query = #acl_is_allowed{
        action = insert,
        object = #acl_edge{ subject_id = SubjectId, object_id = ObjectId, predicate = relation }
    },
    Context
) ->
    case m_rsc:is_a(SubjectId, contribution, Context) andalso m_rsc:is_a(ObjectId, reference, Context) of
        true -> {"insert relation on contribution or reference for member", true};
        _ -> is_allowed_explained(UserGroups, Query, Context)
    end;
% Project managers are allowed to view every resource
is_allowed_explained(
    [<<"acl_user_group_project_manager">> | _UGs],
    #acl_is_allowed{ action = view },
    _Context
) ->
    {"view allowed for project manager", true};
% Project managers are allowed to remove themselves, but no other PMs, from a project
is_allowed_explained(
    [<<"acl_user_group_project_manager">> | _UGs],
    #acl_is_allowed{
        action = delete,
        object = #acl_edge{ object_id = ObjectId, predicate = hascollabmanager }
    },
    Context
) ->
    case ObjectId == z_acl:user(Context) of
        true -> {"project managers can remove themselves from a project", true};
        false -> {"project managers can not remove other project managers than themselves", false}
    end;
% Project managers' permissions on resources extend to the edges that use said resources as subject
is_allowed_explained(
    [<<"acl_user_group_project_manager">> | UserGroups],
    #acl_is_allowed{
        action = Action,
        object = #acl_edge{ subject_id = SubjectId }
    },
    Context
) ->
    is_allowed_explained(
        [<<"acl_user_group_project_manager">> | UserGroups],
        #acl_is_allowed{ action = Action, object = SubjectId },
        Context
    );
% Project managers' permissions on resources:
% - TODO
is_allowed_explained(
    [<<"acl_user_group_project_manager">> | UserGroups],
    Query = #acl_is_allowed{ object = Rsc },
    Context
) when is_integer(Rsc) ->
    UserId = z_acl:user(Context),
    case edge_exists(Rsc, hascollabmanager, UserId, Context) of
        true ->
            is_allowed_explained(UserGroups, Query, Context);
        _ ->
            IsOwner = m_rsc:p_no_acl(Rsc, creator_id, Context) == UserId,
            NoMembers = m_edge:objects(Rsc, hascollabmember, Context) == [],
            case {
                IsOwner andalso NoMembers, % Project managers should be allowed to edit and link collab groups they created as sub-groups, but we don't want them to keep these rights forever.
                is_project_manager_of(Rsc, Context),
                m_rsc:is_a(Rsc, acl_collaboration_group, Context),
                m_rsc:is_a(Rsc, text, Context),
                m_rsc:p_no_acl(Rsc, name, Context)
            } of
                {_NewCollabGroup = true, _, _IsCollabGroup = true, _, _} -> {"project manager can edit own collab groups", true};
                {_, _IsProjectManager = true, _IsCollabGroup = true, _, _} -> {"allowed for project manager on managing collab group", true};
                {_, _IsProjectManager = false, _IsCollabGroup = true, _, _} -> {"project manager does not manage collab group", false};
                {_, _, _, _, _Name = <<"vpro_waag_collaboration">>} -> {"allowed for vpro_waag_collaboration for project manager", true};
                _ -> is_allowed_explained(UserGroups, Query, Context)
            end
    end;
is_allowed_explained([_UserGroup | UserGroups], Query, Context) ->
    is_allowed_explained(UserGroups, Query, Context);
is_allowed_explained(_UserGroups, _Query, _Context) ->
    undefined.


is_project_manager_of(Rsc, Context) when is_integer(Rsc) ->
    edge_exists(project_of(Rsc, Context), hascollabmanager, z_acl:user(Context), Context).

project_of(#acl_rsc{category = acl_collaboration_group} = Rsc, _Context) ->
    Rsc;
project_of(#acl_rsc{props = Props}, Context) ->
    project_of(maps:get(<<"content_group_id">>, Props, undefined), Context);
project_of(Rsc, Context) ->
    case m_rsc:is_a(Rsc, acl_collaboration_group, Context) of
        true -> Rsc;
        _ -> project_of(m_rsc:p_no_acl(Rsc, content_group_id, Context), Context)
    end.

%% @doc Adapted from 'acl_user_groups_checks:acl_is_allowed_prop/3'
%% This will allow:
%% - access to the admin/1 user no matter what
%% - access to every property of a user resource to that same user
%% - access to private properties on non-person resources to anyone
%% - access to private properties on any resource editable by the current user
-spec is_allowed_prop_explained(m_rsc:resource_id(), #acl_is_allowed_prop{}, z:context()) ->
    undefined | {Reason, Verdict} when
        Reason :: string(),
        Verdict :: true | false | undefined.
is_allowed_prop_explained(_Id, _Prop, #context{acl=admin}) ->
    {"is admin", true};
is_allowed_prop_explained(_Id, _Prop, #context{user_id=1}) ->
    {"is user 1", true};
is_allowed_prop_explained(Id, _Prop, #context{user_id=Id}) ->
    {"is self", true};
is_allowed_prop_explained(Id, Prop, Context) ->
    case is_private_property(Prop) of
        true ->
            { "a private but not Person prop, or resource is editable"
            , not m_rsc:is_a(Id, person, Context) orelse z_acl:rsc_editable(Id, Context)
            };
        false ->
            undefined
    end.

% Copy of 'acl_user_groups_checks:is_private_property/1' with the addition of
% 'acl_user_groups_checks:is_always_private_property/1'.
is_private_property(<<"email">>) -> true;
is_private_property(<<"email_raw">>) -> true;
is_private_property(<<"phone">>) -> true;
is_private_property(<<"phone_mobile">>) -> true;
is_private_property(<<"phone_alt">>) -> true;
is_private_property(<<"address_street_1">>) -> true;
is_private_property(<<"address_street_2">>) -> true;
is_private_property(<<"address_postcode">>) -> true;
is_private_property(<<"address_city">>) -> true;
is_private_property(<<"date_start">>) -> true;
is_private_property(<<"date_end">>) -> true;
is_private_property(<<"location_lat">>) -> true;
is_private_property(<<"location_lng">>) -> true;
is_private_property(<<"pivot_location_lat">>) -> true;
is_private_property(<<"pivot_location_lng">>) -> true;
is_private_property(<<"pivot_geocode">>) -> true;
is_private_property(<<"pivot_geocode_qhash">>) -> true;
% These are the clauses from 'is_always_private_property':
is_private_property(<<"billing_email">>) -> true;
is_private_property(<<"billing_street_1">>) -> true;
is_private_property(<<"billing_street_2">>) -> true;
is_private_property(<<"billing_postcode">>) -> true;
is_private_property(<<"billing_city">>) -> true;
is_private_property(<<"billing_state">>) -> true;
is_private_property(<<"billing_country">>) -> true;
is_private_property(_) -> false.

% @doc Utility function to check if there is an edge between two resources
-spec edge_exists(Subject, Predicate, Object, Context) -> boolean() when
    Subject :: m_rsc:resource(),
    Predicate :: m_rsc:resource(),
    Object :: m_rsc:resource(),
    Context :: z:context().
edge_exists(undefined, _PredId, _ObjectId, _Context) -> false;
edge_exists(_SubjectId, undefined, _ObjectId, _Context) -> false;
edge_exists(_SubjectId, _PredId, undefined, _Context) -> false;
edge_exists(SubjectId, PredId, ObjectId, Context) ->
    m_edge:get_id(SubjectId, PredId, ObjectId, Context) =/= undefined.

%% @doc List of ACL rules to be installed with the website.
%% This is used in 'kenniscloud_schema:install_acl_rules/1'.
rules() ->
    [
        % Every member is allowed access to the frontend edit (formerly 'mod_ginger_edit')
        {module, [
            {acl_user_group_id, acl_user_group_members},
            {actions, [use]},
            {module, mod_driebit_edit}
        ]},
        % CL can place users in user groups.
        {module, [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [use]},
            {module, mod_acl_user_groups}
        ]},
        % CL can access the admin too.
        {module, [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [use]},
            {module, mod_admin}
        ]},
        % PM can access the admin too.
        {module, [
            {acl_user_group_id, acl_user_group_project_manager},
            {actions, [use]},
            {module, mod_admin}
        ]},
        % Editors can use seo.
        {module, [
            {acl_user_group_id, acl_user_group_editors},
            {actions, [use]},
            {module, mod_seo}
        ]},
        % Editors can create users and assign usernames/passwords to them.
        {module, [
            {acl_user_group_id, acl_user_group_editors},
            {actions, [use]},
            {module, mod_admin_identity}
        ]},
        % Editors can configure the menu with 'use' access to 'mod_menu'.
        % This is granted already by 'mod_driebit_base', see its 'manage_data'.

        % Member can view all content in content group
        % (overrides deny rules in resource table for hidden collab groups)
        {collab, [
            {actions, [view]},
            {category_id, all}
        ]},
        % Editors can edit all collaboration groups
        {rsc, [
            {acl_user_group_id, acl_user_group_editors},
            {actions, [view, insert, update, delete, link]},
            {category_id, acl_collaboration_group}
        ]},
        % Editors can edit everything in all collaboration groups
        {rsc, [
            {acl_user_group_id, acl_user_group_editors},
            {actions, [view, insert, update, delete, link]},
            {content_group_id, acl_collaboration_group}
        ]},
        % Editors can edit all acl_collaboration_groups (override for own content from CL)
        {rsc, [
            {acl_user_group_id, acl_user_group_editors},
            {actions, [view, insert, update, delete, link]},
            {content_group_id, acl_collaboration_group},
            {category_id, acl_collaboration_group}
        ]},
        % Anonymous view collab groups
        {rsc, [
            {acl_user_group_id, acl_user_group_anonymous},
            {actions, [view]},
            {content_group_id, acl_collaboration_group}
        ]},
        % Members can upload media, for instance a profile picture.
        {rsc, [
            {acl_user_group_id, acl_user_group_members},
            {is_owner, true},
            {actions, [insert, update, delete, link]},
            {category_id, media}
        ]},
        % Member can add tags in user generated content_group
        {rsc, [
            {acl_user_group_id, acl_user_group_members},
            {is_owner, true},
            {actions, [insert, update, link]},
            {category_id, keyword},
            {content_group_id, cg_user_generated}
        ]},
        % Member can create, edit and link own resources in all acl_collaboration_groups
        % Note that that theoretically includes Person resources, though Member has no
        % access to (admin) pages where they can actually create them. Still, this
        % is something to revisit, because as api access evolves this could become
        % problematic after all. This rule is not as it was, before the merge of
        % ACL rule based private collab groups.
        {rsc, [
            {acl_user_group_id, acl_user_group_members},
            {is_owner, true},
            {actions, [insert, update, link]},
            {content_group_id, acl_collaboration_group}
        ]},
        % Member can add, edit and link own contributions in content group
        {collab, [
            {is_owner, true},
            {actions, [insert, update, link]},
            {category_id, contribution}
        ]},
        % Member can add, edit and link own meetups in content group
        {collab, [
            {is_owner, true},
            {actions, [insert, update, link]},
            {category_id, event}
        ]},
        % Member can add, edit and link own references in content group
        {collab, [
            {is_owner, true},
            {actions, [insert, update, link]},
            {category_id, reference}
        ]},
        % Surfaced by the issue that PL's cannot delete their own content:
        % They should have the same rights as CL's in projects they lead.
        % And at least they should be able to remove and edit their own
        % content:
        {rsc, [
            {acl_user_group_id, acl_user_group_project_manager},
            {actions, [view, update, delete]},
            {content_group_id, acl_collaboration_group},
            {is_owner, true}
        ]},
        % 'is_allowed' assumes Project leaders only have extra rights (above members) on cg's, projects and texts
        %
        % Project leader can edit cg's of projects they lead, further limited by 'is_allowed'
        {rsc, [
            {acl_user_group_id, acl_user_group_project_manager},
            {actions, [update]},
            {content_group_id, acl_collaboration_group},
            {category_id, acl_collaboration_group}
        ]},
        % Project leader can edit projects they lead, further limited by 'is_allowed'
        {rsc, [
            {acl_user_group_id, acl_user_group_project_manager},
            {actions, [update]},
            {category_id, acl_collaboration_group}
        ]},
        % Community librarian can view and edit everything in collaboration groups
        {rsc, [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [view, insert, update, delete, link]},
            {content_group_id, acl_collaboration_group},
            {is_owner, true}
        ]},
        % Community librarian can add acl_collaboration_group in standard default
        % group otherwise they can't make a group
        {rsc, [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [insert]},
            {content_group_id, default_content_group},
            {category_id, acl_collaboration_group}
        ]},
        % Project leader can add a collaboration group in the standard default
        % group otherwise they can't make a group
        {rsc, [
            {acl_user_group_id, acl_user_group_project_manager},
            {actions, [insert]},
            {content_group_id, default_content_group},
            {category_id, acl_collaboration_group}
        ]},
        % Community librarian can link acl user groups, allowing them to
        % place users into user groups
        {rsc,
        [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [link]},
            {category_id, acl_user_group}
        ]},
        % Community librarian can (un)link (and used to be able to also edit) members
        {rsc, [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [link]},
            {category_id, person}
        ]},
        % Project leader can (un)link (and used to be able to also edit) members
        {rsc, [
            {acl_user_group_id, acl_user_group_project_manager},
            {actions, [link]},
            {category_id, person}
        ]},
        % Community librarian can add and edit tags
        {rsc, [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [update, insert, delete, link]},
            {content_group_id, default_content_group},
            {category_id, keyword}
        ]},
        % Community librarian can view methodology content
        {rsc,
        [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [view]},
            {content_group_id, cg_methodology}
        ]},
        % Community librarian can change roadmaps
        {rsc, [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [update, link]},
            {content_group_id, cg_methodology},
            {category_id, roadmap}
        ]},
        % Community librarian can change steps
        {rsc, [
            {acl_user_group_id, acl_user_group_community_librarian},
            {actions, [update, insert, link]},
            {content_group_id, cg_methodology},
            {category_id, roadmap_step}
        ]}
    ].


