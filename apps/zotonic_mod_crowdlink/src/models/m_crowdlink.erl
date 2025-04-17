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

-module(m_crowdlink).

-export([
    m_get/3,

    set/2,
    is_valid/3,
    url/2,
    get_all_valid/1,
    clean_old/1,
    delete/2,
    expiry/2,
    set_expiry/2,

    set_daycrowds/1,
    set_daycrowd/2,
    set_daycrowd/3,

    install/1
]).

-behaviour(zotonic_model).
-include("zotonic_core/include/zotonic.hrl").


-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().

% Syntax: m.crowdlink.all
% Returns all the active links
m_get([ <<"all">> | Rest ], _Msg, Context) ->
    case user_view_overview_crowdlink(Context) of
        true ->
            {ok, {get_all_valid(Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdlink.for_meetup[meetup_id].expiry
m_get([ <<"for_meetup">>, MeetupId, <<"expiry">> | Rest ], _Msg, Context) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {get_link_expires(MeetupId, Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdlink.for_meetup[meetup_id].code
m_get([ <<"for_meetup">>, MeetupId, <<"code">> | Rest ], _Msg, Context) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {get_link_code(MeetupId, Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdlink.for_meetup[meetup_id].url
m_get([ <<"for_meetup">>, MeetupId, <<"url">> | Rest ], _Msg, Context) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {url(MeetupId, Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdlink.for_meetup[meetup_id].has_link
m_get([ <<"for_meetup">>, MeetupId, <<"has_link">> | Rest ], _Msg, Context) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {has_link(MeetupId, Context), Rest}};
        false ->
            {error, eacces}
    end;

% Unexpected query
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.


%% Get all the active links
-spec get_all_valid(z:context()) -> [{binary(), integer(), calendar:datetime()}].
get_all_valid(Context) ->
    z_db:q("SELECT url, crowd_id, expires FROM crowdlink WHERE expires > now()", [], Context).

-spec install(z:context()) -> ok.
install(Context) ->
    case z_db:table_exists(crowdlink, Context) of
        false ->
            [] = z_db:q("
                CREATE TABLE crowdlink (
                    crowd_id SERIAL NOT NULL PRIMARY KEY,
                    url VARCHAR(16),
                    expires TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now() + INTERVAL '3 day'
                )",
                Context
            ),
            ok;
        true ->
            ok
    end.

% PURGE:

%% Create a link to a Crowd
%% The version of this code using "ON CONFLICT .. UPDATE" unfortunately is only available
%% from Postgresql 9.6. The recommended (safest) alternative is using stored procedures, see
%% https://www.postgresql.org/docs/current/plpgsql-control-structures.html#PLPGSQL-UPSERT-EXAMPLE
%% Probably the best trade-off between this and practical use is to use the z_db transaction
%% function performing possibly indefinite retries on various error conditions (~:scream:).
set(CrowdId, Context) ->
    Url = z_ids:id(16),
    z_db:transaction(
        fun (DbOnlyContext) ->
            Expiry = expiry(CrowdId, Context),
            RowsAffected = z_db:q(
                "UPDATE crowdlink SET url = $2, expires = $3 WHERE crowd_id = $1",
                [CrowdId, Url, Expiry],
                DbOnlyContext
            ),
            case RowsAffected of
            0 ->
                z_db:q(
                    "INSERT INTO crowdlink (crowd_id, url, expires) VALUES ($1, $2, $3)",
                    [CrowdId, Url, Expiry],
                    DbOnlyContext
                );
            _ ->
                RowsAffected
            end
        end,
        Context
    ).

%% Expiry based on the end date time of a crowd event
expiry(CrowdId, Context) ->
    expiry(m_crowd:category(CrowdId, Context), CrowdId, Context).

expiry(crowd, CrowdId, Context) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(
            case m_rsc:p_no_acl(CrowdId, date_end, Context) of
                undefined -> calendar:universal_time();
                DateEnd -> DateEnd
            end
        ) + 72*3600
    );
expiry(daycrowd, CrowdId, Context) ->
    StartDate = case m_rsc:p_no_acl(CrowdId, date_start, Context) of
        undefined ->
            {Date, _} = z_datetime:to_datetime(<<"now">>),
            Date;
        {Date, _} ->
            Date
    end,
    z_datetime:next_day(StartDate).

set_expiry(CrowdId, Context) ->
    z_db:q(
        "UPDATE crowdlink SET expires = $2 WHERE crowd_id = $1",
        [CrowdId, expiry(CrowdId, Context)],
        Context
    ).

%% Check if link is valid
is_valid(CrowdId, Url, Context) ->
    undefined =/= z_db:assoc_row(
        "SELECT * FROM crowdlink WHERE url = $1 AND crowd_id = $2 AND expires > now()",
        [Url, CrowdId],
        Context
    ).

%% Get the full crowdlink url
url(MeetupId, Context) ->
    case get_link_code(MeetupId, Context) of
        undefined -> undefined;
        Code ->
            CrowdIdStr = z_convert:to_list(MeetupId),
            z_dispatcher:abs_url(
                z_dispatcher:url_for(crowd, [{id, CrowdIdStr}, {crowdlink, Code}], Context),
                Context
            )
    end.

%% Get active links for particular Crowd
-spec get_link_code( integer(), z:context() ) -> binary() | undefined.
get_link_code(CrowdId, Context) ->
    z_db:q1(
        "SELECT url FROM crowdlink WHERE crowd_id = $1 AND expires > now()",
        [CrowdId],
        Context
    ).

-spec get_link_expires( integer(), z:context() ) -> calendar:datetime() | undefined.
get_link_expires(CrowdId, Context) ->
    z_db:q1(
        "SELECT expires FROM crowdlink WHERE crowd_id = $1 AND expires > now()",
        [CrowdId],
        Context
    ).

%% Get active links for particular Crowd
has_link(CrowdId, Context) ->
    undefined =/= z_db:q1(
        "SELECT crowd_id FROM crowdlink WHERE crowd_id = $1 AND expires > now()",
        [CrowdId],
        Context
    ).

delete(CrowdId, Context) ->
    z_db:q("DELETE FROM crowdlink WHERE crowd_id = $1", [CrowdId], Context).

clean_old(Context) ->
    % delete crowdlinks that are expiring in the next 1h30m - added 30 minutes to
    % account for edge cases and better expire half an hour early than too late
    % in this case
    z_db:q("DELETE FROM crowdlink WHERE now() > (expires - interval '1 hour 30 minutes')", Context).

set_daycrowds(Context) ->
    % - find daycrowds, and per daycrowd
    % - if date_start is before today
    % - set start_date to next day
    % - set expiry
    {Today, _} = z_datetime:to_datetime(<<"now">>),
    m_category:foreach(
        daycrowdevent,
        fun(Rsc, Ctx) ->
            case {m_rsc:p_no_acl(Rsc, is_published, Ctx), m_rsc:p_no_acl(Rsc, date_start, Ctx)} of
                {true, {Date, _}} when Date >= Today -> ok;
                {true, _} -> set_daycrowd(Rsc, Today, Ctx);
                _ -> ok
            end
        end,
        z_acl:sudo(Context)
    ).

set_daycrowd(Rsc, Context) ->
    {Today, _} = z_datetime:to_datetime(<<"now">>),
    case m_rsc:p_no_acl(Rsc, date_start, Context) of
        {Date, Time} when Date >= Today -> set_daycrowd(Rsc, {Date, Time}, Context);
        _ -> set_daycrowd(Rsc, Today, Context)
    end.

set_daycrowd(Rsc, {{Y, M, D}, _Time} = Start, Context) ->
    z:info("Preparing/resetting daycrowd ~p starting ~p", [Rsc, {Y, M, D}], [], Context),
    remove_crowd_attendance(Rsc, Context),
    End = z_datetime:next_day({Y, M, D}),
    m_rsc:update(Rsc, [{date_start, Start}, {date_end, End}], Context),
    set(Rsc, Context);
set_daycrowd(Rsc, {Y, M, D}, Context) ->
    set_daycrowd(Rsc, {{Y, M, D}, {0, 0, 0}}, Context).

remove_crowd_attendance(Rsc, Context) ->
    lists:foreach(
        fun(Subject) ->
            m_edge:delete(Subject, rsvp, Rsc, Context)
        end,
        m_edge:subjects(Rsc, rsvp, Context)
    ).

% can view admin template with listing of all information
user_view_overview_crowdlink(Context) ->
    z_acl:is_allowed(use, mod_crowdlink, Context).

% can view admin template with info by event
user_view_crowdlink(Context) ->
    z_acl:is_allowed(use, mod_admin, Context).
