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

%% @doc Bridge between Kenniscloud and mod_ginger_activity.
-module(kenniscloud_activity).

-export([
    notifications_seen_at/2,
    seen_notifications/1,
    register_like/3,
    undo_like/3,

    maybe_register_activity/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec notifications_seen_at(m_rsc:resource(), z:context()) -> calendar:datetime() | undefined.
notifications_seen_at(User, Context) ->
    m_rsc:p(User, notifications_seen_at, Context).

-spec seen_notifications(z:context()) -> {ok, m_rsc:resource()} | {error, term()}.
seen_notifications(Context) ->
    m_rsc:update(
        z_acl:user(Context),
        #{notifications_seen_at => calendar:universal_time()},
        Context
    ).


register_like(UserId, ObjectId, Context) ->
    m_activity:register(
        like,
        [{object, ObjectId}],
        z_acl:logon(UserId, Context)
    ).

undo_like(UserId, ObjectId, Context) ->
    % From the activities of the user, find a 'like' one that is still published and has an edge with the object:
    case m_activity:find(like, [{actor, UserId}, {object, ObjectId}], Context) of
        [ActivityId | _Rest] ->
            % insert an 'undo' activity as the user:
            % Note: mod_driebit_activity will unpublish the like too
            m_activity:register(
                undo,
                [{object, ActivityId}],
                z_acl:logon(UserId, Context)
            );
        _ ->
            ok
    end.

%% @doc Maybe register an activity for something that happened on Kenniscloud.
-spec maybe_register_activity(m_rsc:resource(), z:context()) -> ok.
maybe_register_activity(Rsc, Context) ->
    case m_rsc:p(Rsc, is_published, Context) of
        true ->
            case m_rsc:is_a(Rsc, remark, Context) of
                true ->
                    Contribution = m_remarks:contribution(Rsc, Context),
                    maybe_register_remark_activity(Rsc, Contribution, Context);
                false ->
                    maybe_register_contribution_activity(Rsc, Context)
            end;
        false ->
            z:info("No activity registration for ~p (rsc is not published)", [Rsc], #{}, Context),
            nop
    end,
    ok.

%% @doc Register an activity when a user adds a remark to a contribution.
-spec maybe_register_remark_activity(m_rsc:resource(), m_rsc:resource(), z:context()) -> ok | nop.
maybe_register_remark_activity(Rsc, Rsc, Context) ->
    z:info("No activity registration for remark ~p (no contribution found)", [Rsc], #{}, Context),
    nop;
maybe_register_remark_activity(Rsc, About, Context) ->
    BaseInfo = [{object, Rsc}, {target, About}],
    Recipients = [{to, IdTo} || IdTo <- m_remarks:recipients(Rsc, Context)],
    Interested = [{cc, IDCc} || IDCc <- interested_users(Rsc, Context)],

    m_activity:register(
        create,
        BaseInfo ++ Recipients ++ Interested,
        Context
    ).

%% @doc Register an activity when a user adds a contribution to a knowledge group.
-spec maybe_register_contribution_activity(m_rsc:resource(), z:context()) -> ok | nop.
maybe_register_contribution_activity(Rsc, Context) ->
    case is_contribution(Rsc, Context) of
        true ->
            case knowledge_group(Rsc, Context) of
                undefined ->
                    z:info("No activity registration for ~p (not in a kennisgroep)", [Rsc], #{}, Context),
                    nop;
                Target ->
                    BaseInfo = [{object, Rsc}, {target, Target}],
                    Interested = [{cc, IDCc} || IDCc <- interested_users(Rsc, Context)],
                    m_activity:register(
                        create,
                        BaseInfo ++ Interested,
                        Context
                    )
            end;
        false ->
            z:info("No activity registration for ~p (is not a contribution)", [Rsc], #{}, Context),
            nop
    end.

%% @doc Is the resource a contribution?
-spec is_contribution(m_rsc:resource(), z:context()) -> boolean().
is_contribution(Rsc, Context) ->
    m_rsc:is_a(Rsc, contribution, Context)
        orelse m_rsc:is_a(Rsc, event, Context)
        orelse m_rsc:is_a(Rsc, reference, Context).

%% @doc Get users that are interested in an activity and should get it in their inbox.
-spec interested_users(m_rsc:resource(), z:context()) -> [m_rsc:resource()].
interested_users(Rsc, Context) ->
    case m_rsc:is_a(Rsc, remark, Context) of
        true ->
            interested_in_remark(Rsc, Context);
        false ->
            case is_contribution(Rsc, Context) of
                true ->
                    interested_in_contribution(Rsc, Context);
                false ->
                    []
            end
    end.

%% @doc Get knowledge group that a resource belongs to.
-spec knowledge_group(m_rsc:resource(), z:context()) -> m_rsc:resource() | undefined.
knowledge_group(Rsc, Context) ->
    ContentGroup = m_rsc:p(Rsc, content_group_id, Context),
    case m_rsc:is_a(ContentGroup, acl_collaboration_group, Context) of
        true ->
            ContentGroup;
        false ->
            undefined
    end.

%% @doc Users are interested in a remark on a contribution when:
%%      - they created the contribution; or
%%      - they themselves have added a remark to the contribution.
-spec interested_in_remark(m_rsc:resource(), z:context()) -> [integer()].
interested_in_remark(Remark, Context) ->
    interested_in_remarks_on(m_remarks:contribution(Remark, Context), Context).

-spec interested_in_remarks_on(m_rsc:resource(), z:context()) -> [integer()].
interested_in_remarks_on(Contribution, Context) ->
    ContributionCreator = m_rsc:p_no_acl(Contribution, creator_id, Context),
    RemarkCreators = lists:map(
        fun(Remark) ->
            m_rsc:p_no_acl(Remark, creator_id, Context)
        end,
        m_kc_contribution:remarks(Contribution, Context)
    ),
    lists:usort([ContributionCreator | RemarkCreators]).

%% @doc Users are interested in a contribution when:
%%      - they are member of the contribution's knowledge group.
-spec interested_in_contribution(m_rsc:resource(), z:context()) -> [m_rsc:resource()].
interested_in_contribution(Contribution, Context) ->
    Group = knowledge_group(Contribution, Context),
    lists:usort(m_edge:objects(Group, hascollabmember, Context)).
