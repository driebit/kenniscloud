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

%% @doc Sending notification emails.
-module(kenniscloud_notifications).

-export([
    fan_out/4,
    unsubscribe/1,
    timestamp/0
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec fan_out(m_rsc:resource(), [m_rsc:resource()], [m_rsc:resource()], z:context()) -> ok.
fan_out(Rsc, Interested, Mentions, Context) ->
    case m_rsc:is_a(Rsc, remark, Context) of
        false ->
            ok;
        true ->
            z:info("Notifications: fan out started for remark ~p", [Rsc], #{}, Context),
            lists:foreach(
                fun(User) ->
                    send_notification_of_remark(Rsc, User, <<"Er is gereageerd">>, Context)
                end,
                allowed_recipients(Interested, Context)
            ),
            lists:foreach(
                fun(User) ->
                    send_notification_of_remark(Rsc, User, <<"Je bent genoemd in deze reactie">>, Context)
                end,
                allowed_recipients(Mentions, Context)
            ),
            ok
    end.

timestamp() ->
    {M,S,_M} = os:timestamp(),
    M*1000000 + S.

last_notification_mail_sent_at(User, Context) ->
    m_rsc:p(User, notification_mail_last_sent_at, undefined, Context).

set_notification_mail_sent_at(User, Context) ->
    m_rsc:update(User, [ {notification_mail_last_sent_at, timestamp()} ], Context).

notification_mail_in_cool_down_period(User, Seconds, Context) ->
    case last_notification_mail_sent_at(User, Context) of
        undefined ->
            false;
        LastSent ->
            Now = timestamp(),
            (Now - LastSent) =< Seconds
    end.

daily_notification_mail_count(User, Context) ->
    m_rsc:p(User, notification_mail_count, 0, Context).

daily_maximum_notification_mail_count_reached(User, MaxCount, Context) ->
    case last_notification_mail_sent_at(User, Context) of
        undefined ->
            false;
        LastSent ->
            Now = timestamp(),
            case (Now - LastSent >= 60*60*24) of
                true ->
                    m_rsc:update(User, [ {notification_mail_count, 0} ], Context),
                    false;
                false ->
                    daily_notification_mail_count(User, Context) >= MaxCount
            end
    end.

increase_notification_mail_count(User, Context) ->
    Count = daily_notification_mail_count(User, Context),
    m_rsc:update(User, [ {notification_mail_count, Count + 1} ], Context).

%% @doc Filter a list of user(id)s to only include those ok to send out a notification to now
-spec allowed_recipients([m_rsc:resource()], z:context()) -> [m_rsc:resource()].
allowed_recipients(Users, Context) ->
    lists:filter(
        fun(User) ->
            may_send_notification_to(User, Context)
        end,
        Users
    ).

%% @doc Indicate whether it would be ok to send out a notification (to avoid being spammy)
%%      This makes use of a few properties in the User resource:
%%      - receive_notification_mail
%%      - notification_mail_count
%%      - notification_mail_last_sent_at
-spec may_send_notification_to(m_rsc:resource(), z:context()) -> boolean().
may_send_notification_to(User, Context) ->
    % Don't send mails if the user indicated on their profile that they don't like to get notified
    % Don't send more than one mail per hour
    % Don't send more than four mails per day
    case {m_rsc:p_no_acl(User, receive_notification_mail, Context),
          notification_mail_in_cool_down_period(User, binary_to_integer(m_config:get_value(kenniscloud, email_interval_seconds, <<"3600">>, Context)), Context),
          daily_maximum_notification_mail_count_reached(User, binary_to_integer(m_config:get_value(kenniscloud, email_max_amount_per_day, <<"2">>, Context)), Context)} of
        {undefined, false, false} ->
            true;
        {true, false, false} ->
            true;
        {ReceiveNotifEmail, InCooldown, DailyMax} ->
            z:info("Notifications: not sending to user ~p (subscribed: ~p, in cooldown: ~p, reached daily max: ~p)", [User, ReceiveNotifEmail, InCooldown, DailyMax], #{}, Context),
            false
    end.

%% @doc Send out a user notification about remarks (mentions, posts) via email.
-spec send_notification_of_remark(m_rsc:resource(), m_rsc:resource(), binary(), z:context()) -> ok.
send_notification_of_remark(Remark, User, Message, Context) ->
    Topic = m_remarks:contribution(Remark, Context),
    TopicTitle = m_rsc:p(Topic, title, Context),
    RemarkContent = m_rsc:p(Remark, body, Context),
    Content = [
        [Message, <<" op \"">>, TopicTitle, <<"\":">>],
        [<<"\"">>, RemarkContent, <<"\"">>]
    ],
    TopicUrl = z_convert:to_binary(m_rsc:page_url_abs(Topic, Context)),
    RemarkIdBin = z_convert:to_binary(Remark),
    Link = <<TopicUrl/binary, "#", RemarkIdBin/binary>>,
    send_notification_email(User, TopicTitle, Content, Link, Context).

%% @doc Send out a notification email to a single user.
-spec send_notification_email(m_rsc:resource(), m_rsc:resource(), binary(), binary(), z:context()) -> ok.
send_notification_email(User, Topic, Content, Link, Context) ->
    Email = m_rsc:p(User, email, Context),
    UnsubscribeUrl =
        z_dispatcher:url_for(
          unsubscribe,
          [
           {mailing, notifications},
           {email, Email},
           {tm, z_datetime:timestamp()},
           {z_access_url, true},
           {absolute_url, true}
          ],
          z_acl:logon(User, Context)),
    Vars = [
        {recipient_id, User},
        {user_id, User},
        {email, Email},
        {topic, Topic},
        {content, Content},
        {link, Link},
        {unsubscribe_url, UnsubscribeUrl}
    ],
    set_notification_mail_sent_at(User, Context),
    increase_notification_mail_count(User, Context),
    z_email:send_render(Email, "email_notification.tpl", Vars, z_acl:sudo(Context)),
    ok.

unsubscribe(Context) ->
    User = z_acl:user(Context),
    m_rsc:update(User, [{receive_notification_mail, false}], Context).
