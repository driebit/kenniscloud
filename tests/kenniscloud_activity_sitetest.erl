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

-module(kenniscloud_activity_sitetest).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_driebit_activity2/src/include/driebit_activity2.hrl").

notification_inbox_test() ->
    Context = context(),
    Dirk = z_acl:logon(person_dirk, Context),

    %% No activities in inbox to begin with.
    m_driebit_activity2_inbox:delete(dirk(), Context),
    m_driebit_activity2_inbox:delete(frederike(), Context),
    ?assertEqual(undefined, m_rsc:p(frederike(), notification_mail_last_sent_at, Context)),

    {ok, EventId} = m_rsc:insert(
        [
            {category, event},
            {content_group_id, page_nieuwestadmaken},
            {title, <<"Nieuwe meetup!">>},
            {is_published, true}
        ],
        Dirk
    ),

    %% Wait for kenniscloud:observe_rsc_update_done/2 and observe_driebit_activity2_inserted/2.
    timer:sleep(1000),

    %% Creator of contribution must not get notification.
    ?assertEqual([], m_driebit_activity2_inbox:for_user(dirk(), Dirk)),

    %% Other members of knowledge group must get notification.
    [Activity] = m_driebit_activity2_inbox:for_user(frederike(), Context),
    ?assertEqual(EventId, Activity#driebit_activity2.rsc_id),
    ?assertEqual(dirk(), Activity#driebit_activity2.user_id),

    %% No e-mail must be sent to knowledge group members for new contributions.
    ?assertEqual(undefined, m_rsc:p(frederike(), notification_mail_last_sent_at, Context)).

notification_publish_flow_test() ->
    Context = context(),
    Dirk = z_acl:logon(person_dirk, Context),

    m_driebit_activity2_inbox:delete(dirk(), Context),
    m_driebit_activity2_inbox:delete(frederike(), Context),

    %% No activities in inbox to begin with.
    ?assertEqual([], m_driebit_activity2_inbox:for_user(dirk(), Context)),
    ?assertEqual([], m_driebit_activity2_inbox:for_user(frederike(), Context)),

    {ok, EventId} = m_rsc:insert(
        [
            {category, event},
            {content_group_id, page_nieuwestadmaken},
            {title, <<"Nieuwe meetup!">>},
            {is_published, true}
        ],
        Dirk
    ),

    {ok, EventId} = m_rsc:update(
        EventId,
        [
            {title, <<"Nieuwe titel!">>}
        ],
        Dirk
    ),

    %% Wait for kenniscloud:observe_rsc_update_done/2 and observe_driebit_activity2_inserted/2.
    timer:sleep(1000),

    ?assertEqual([], m_driebit_activity2_inbox:for_user(dirk(), Dirk)),
    Inbox = m_driebit_activity2_inbox:for_user(frederike(), Context),
    ?assertEqual(1, length(Inbox)),
    [Activity] = Inbox,
    ?assertEqual(EventId, Activity#driebit_activity2.rsc_id),
    ?assertEqual(dirk(), Activity#driebit_activity2.user_id).

remark_notification_test() ->
    Context = context(),
    Dirk = z_acl:logon(person_dirk, Context),
    Frederike = z_acl:logon(person_frederike, Context),

    %% Dirk creates a contribution.
    {ok, ContributionId} = m_rsc:insert(
        [
            {category, event},
            {content_group_id, page_nieuwestadmaken},
            {title, <<"Nieuwe bijdrage!">>},
            {is_published, true}
        ],
        Dirk
    ),

    timer:sleep(1000),

    %% No activities in inbox to begin with.
    m_driebit_activity2_inbox:delete(dirk(), Context),
    m_driebit_activity2_inbox:delete(frederike(), Context),

    %% Frederike remarks on it.
    Remark = #{
        <<"rsc">> => #{
            <<"category">> => <<"remark">>
        },
        <<"edges">> => [
            #{
                <<"object_id">> => ContributionId,
                <<"predicate">> => <<"about">>
            }
        ]
    },
    controller_update:process_post(#wm_reqdata{bodyfetch = Remark}, Frederike),
    timer:sleep(1000),

    %% Creator of remark (Frederike) must not get notification.
    ?assertEqual([], m_driebit_activity2_inbox:for_user(frederike(), Frederike)),

    %% Creator of contribution (Dirk) must get a notification.
    [Activity] = m_driebit_activity2_inbox:for_user(dirk(), Dirk),
    ?assertEqual(ContributionId, Activity#driebit_activity2.target_id),
    ?assertEqual(frederike(), Activity#driebit_activity2.user_id).

context() ->
    Context = z:c(kenniscloud),
    start_modules(Context),
    Context.

start_modules(Context) ->
    ok = z_module_manager:activate_await(kenniscloud, Context).

frederike() ->
    m_rsc:rid(person_frederike, context()).

dirk() ->
    m_rsc:rid(person_dirk, context()).
