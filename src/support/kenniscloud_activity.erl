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
    maybe_register_activity/2,
    fan_out/2,
    interested_users/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_driebit_activity2/src/include/driebit_activity2.hrl").

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

%% @doc Fan out an activity to all interested users' activity stream inboxes.
-spec fan_out(mod_driebit_activity2:activity(), z:context()) -> ok.
fan_out(Activity, Context) ->
    z:info("Activity fan out: ~p", [Activity], #{}, Context),
    #driebit_activity2{user_id = User, rsc_id = Rsc, to = To} = Activity,
    Users = interested_users(Rsc, Context),
    %% Don't notify users of their own actions.
    Interested = lists:delete(User, Users),
    InterestedWithTo = lists:usort(Interested ++ To),
    % Caution below: -- is expensive, slow for long lists; maybe use ordsets instead
    kenniscloud_notifications:fan_out(Rsc, Interested -- To, To, Context),
    m_driebit_activity2_inbox:fan_out(Activity, InterestedWithTo, Context).

-spec considered_same_activity(integer(), undefined | calendar:datetime(), calendar:datetime()) -> boolean().
considered_same_activity(_TTL, undefined, _NewTime) ->
    false;
considered_same_activity(ActivityTTL_sec, ActivityTime, NewTime) ->
    (calendar:datetime_to_gregorian_seconds(NewTime) - calendar:datetime_to_gregorian_seconds(ActivityTime)) < ActivityTTL_sec.


%% @doc Throttle activity logging to avoid having to include (way) less simple conditions on the log triggers.
-spec register_activity_throttled(mod_driebit_activity2:activity(), z:context()) -> ok | nop.
register_activity_throttled(Activity, Context) ->
    #driebit_activity2{user_id = User, rsc_id = Rsc, time = NewTime} = Activity,
    % Don't insert if for the same rsc there was already something logged very recently, defaulting to 60sec because this is also the displayed time granularity.
    LastTime = z_db:q1("SELECT time FROM activity_log WHERE rsc_id = $1 AND user_id = $2 ORDER BY time DESC LIMIT 1", [Rsc, User], Context),
    ActivityTTL_sec = z_convert:to_integer(m_config:get_value(kenniscloud, notifications_ttl_sec, 60, Context)),
    case considered_same_activity(ActivityTTL_sec, LastTime, NewTime) of
        false ->
            mod_driebit_activity2:register_activity(Activity, Context),
            ok;
        true ->
            z:info("No activity registration for ~p (throttled)", [Activity], #{}, Context),
            nop
    end.


%% @doc Register an activity when a user adds a remark to a contribution.
-spec maybe_register_remark_activity(m_rsc:resource(), m_rsc:resource(), z:context()) -> ok | nop.
maybe_register_remark_activity(Rsc, Rsc, Context) ->
    z:info("No activity registration for remark ~p (no contribution found)", [Rsc], #{}, Context),
    nop;
maybe_register_remark_activity(Rsc, About, Context) ->
    Activity = m_driebit_activity2:activity(Rsc, Context),
    WithTarget = m_driebit_activity2:target(Activity, About),
    WithRecipients = m_driebit_activity2:to(WithTarget, m_remarks:recipients(Rsc, Context)),
    register_activity_throttled(WithRecipients, Context).

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
                    Activity = m_driebit_activity2:activity(Rsc, Context),
                    WithTarget = m_driebit_activity2:target(Activity, Target),
                    register_activity_throttled(WithTarget, Context)
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
