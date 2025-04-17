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

-module(m_crowdparticipant).

-export([
    m_get/3,
    m_post/3,

    get_all/1,
    set/2,
    get/2,
    get_by_email_for_crowd/3,
    set_expiries_for_crowd/2,
    delete/2,
    process_expiries_and_send_mails/1,
    register_for_crowd/3,
    send_confirmation_mail_for_crowd/3,

    install/1
]).

-behaviour(zotonic_model).
-include("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().

% Syntax: m.crowdparticipant.all
m_get([ <<"all">> | Rest ], _Msg, Context) ->
    case user_view_overview_crowdparticipant(Context) of
        true ->
            {ok, {get_all(Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdparticipant.daycrowds
m_get([ <<"daycrowds">> | Rest ], _Msg, Context) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {participations_in_session(Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdparticipant.for_meetup[meetup_id]
m_get([ <<"for_meetup">>, MeetupId | Rest ], _Msg, Context) ->
    case user_view_overview_crowdparticipant(Context) of
        true ->
            {ok, {get_all(MeetupId, Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdparticipant[id].has_data
m_get([ Id, <<"has_data">> | Rest], _Msg, Context) when is_integer(Id) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {get(Id, Context) =/= #{}, Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdparticipant[id].name
m_get([ Id, <<"name">> | Rest], _Msg, Context) when is_integer(Id) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {get_field(Id, name, Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdparticipant[id].email
m_get([ Id, <<"email">> | Rest], _Msg, Context) when is_integer(Id) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {get_field(Id, email, Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdparticipant[id].show_contact
m_get([ Id, <<"show_contact">> | Rest], _Msg, Context) when is_integer(Id) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {get_field(Id, show_contact, Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdparticipant[id].expires
m_get([ Id, <<"expires">> | Rest], _Msg, Context) when is_integer(Id) ->
    case user_view_crowdlink(Context) of
        true ->
            {ok, {get_field(Id, expires, Context), Rest}};
        false ->
            {error, eacces}
    end;

% Syntax: m.crowdparticipant.participates_in[crowd_id]
m_get([ <<"participates_in">>, CrowdId | Rest], _Msg, Context) ->
    % checked ok
    {ok, {has_participation_in_session(CrowdId, Context), Rest}};

% Unexpected query
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) ->
    {ok, term()} | ok | {error, term()}.

% API path: /api/model/remarks/post/data/
m_post([<<"data">> | _Rest ], #{ payload := Data }, Context) when is_map(Data) ->
    Crowd = maps:get(<<"crowd">>, Data, undefined),

    case m_rsc:is_a(Crowd, event, Context) of
        false -> {error, enoent};
        true ->
            case register_for_crowd(Crowd, Data, Context) of
                {error, Error} ->
                    ?zError("POST registering for Crowd ~p has error: ~p", [Crowd, Error], Context),
                    {error, Error};
                {ok, Id} ->
                    {ok, [{id, Id}]}
            end
    end;

% Unexpected query
m_post(_Path, _Payload, _Context) ->
    {error, unknown_path}.


-spec install(z:context()) -> ok.
install(Context) ->
    case z_db:table_exists(crowdparticipant, Context) of
        false ->
            [] = z_db:q("
                CREATE TABLE crowdparticipant (
                    person_id INTEGER NOT NULL PRIMARY KEY,
                    crowd_id INTEGER NOT NULL,
                    expires TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now() + INTERVAL '3 day',
                    name VARCHAR(255),
                    email VARCHAR(255),
                    show_contact boolean DEFAULT NULL
                )", Context),
            ok;
        true ->
            [] = z_db:q("DO $$BEGIN ALTER TABLE crowdparticipant ADD COLUMN show_contact boolean DEFAULT NULL; EXCEPTION WHEN duplicate_column THEN RAISE NOTICE 'Pgsql<9.6 compatible skip: column already exists'; END$$;", Context),
            [] = z_db:q("ALTER TABLE crowdparticipant DROP COLUMN IF EXISTS email_in_profile", Context),
            ok
    end.


%% Create a Crowd participant, with expiring data
%% The version of this code using "ON CONFLICT .. UPDATE" unfortunately is only available
%% from Postgresql 9.6. The recommended (safest) alternative is using stored procedures, see
%% https://www.postgresql.org/docs/current/plpgsql-control-structures.html#PLPGSQL-UPSERT-EXAMPLE
%% Probably the best trade-off between this and practical use is to use the z_db transaction
%% function performing possibly indefinite retries on various error conditions (~:scream:).
set(#{person := PersonId, crowd := CrowdId, name := Name, email := Email, show_contact := ShowContact}, Context) ->
    z_db:transaction(
        fun (DbOnlyContext) ->
            Expiry = m_crowdlink:expiry(CrowdId, Context),
            RowsAffected =
                  z_db:q("UPDATE crowdparticipant
                         SET crowd_id = $2, expires = $3, name = $4, email = $5, show_contact = $6
                         WHERE person_id = $1",
                         [PersonId, CrowdId, Expiry, Name, Email, ShowContact],
                         DbOnlyContext),
              if (RowsAffected == 0) ->
                      z_db:q("INSERT INTO crowdparticipant
                             (person_id, crowd_id, expires, name, email, show_contact)
                             VALUES ($1, $2, $3, $4, $5, $6)",
                             [PersonId, CrowdId, Expiry, Name, Email, ShowContact],
                             DbOnlyContext)
              end
      end,
      Context).

%% Get temporarily stored data for a particular participant
-spec get( integer(), z:context() ) -> map().
get(PersonId, Context) ->
    case z_db:assoc_row(
        "SELECT person_id, crowd_id, name, email, show_contact, expires FROM crowdparticipant WHERE person_id = $1",
        [PersonId],
        Context
    ) of
        L when is_list(L) -> maps:from_list(L);
        _ -> #{}
    end.

%% Get a field from the temporarily stored data for a particular participant
-spec get_field( integer(), term(), z:context() ) -> map().
get_field(PersonId, Field, Context) ->
    maps:get(Field, get(PersonId, Context), undefined).

%% Get participants with temporarily stored data for particular Crowd
get_all(CrowdId, Context) ->
    z_db:assoc_props(
        "SELECT person_id, crowd_id, name, email, show_contact, expires FROM crowdparticipant WHERE crowd_id = $1",
        [CrowdId],
        Context
    ).

%% Get all participants with temporarily stored data for any Crowd
get_all(Context) ->
    z_db:q(
        "SELECT person_id, crowd_id, name, email, show_contact, expires FROM crowdparticipant",
        [],
        Context
    ).

get_by_email_for_crowd(Crowd, Email, Context) when is_integer(Crowd) ->
    case z_db:assoc_row(
        "SELECT person_id, crowd_id, name, email, show_contact, expires FROM crowdparticipant WHERE crowd_id = $1 AND email = $2",
        [Crowd, Email],
        Context
    ) of
        L when is_list(L) -> maps:from_list(L);
        _ -> undefined
    end.

set_expiries_for_crowd(CrowdId, Context) ->
    z_db:q(
        "UPDATE crowdparticipant SET expires = $2 WHERE crowd_id = $1",
        [CrowdId, m_crowdlink:expiry(CrowdId, Context)],
        Context
    ).

process_expiries_and_send_mails(Context) ->
    lists:foreach(
        fun (Participant) ->
            PersonId = proplists:get_value(person_id, Participant),
            Email = proplists:get_value(email, Participant),
            Name = proplists:get_value(name, Participant, <<"KennisCrowd-deelnemer">>),
            Crowd = proplists:get_value(crowd_id, Participant),
            delete(PersonId, Context),
            send_expiry_mail(PersonId, Email, Name, Crowd, Context)
        end,
        z_db:assoc("SELECT * FROM crowdparticipant WHERE expires < now() - interval '1 hour'", Context)
    ).

delete(PersonId, Context) ->
    z_db:q("DELETE FROM crowdparticipant WHERE person_id = $1", [PersonId], Context).

send_expiry_mail(PersonId, undefined, _, _, Context) ->
    z:warning(
        "Crowd participant data expiry: No mail sent for person ~p because no email was given",
        [PersonId],
        [],
        Context
    );
send_expiry_mail(PersonId, Email, Name, Crowd, Context) ->
    Vars = [
        {name, Name},
        {crowd, Crowd}
    ],
    z_email:send_render(Email, "email_expiry.tpl", Vars, z_acl:sudo(Context)),
    z:info(
        "Crowd participant data expiry: Mail sent for person ~p",
        [PersonId],
        [],
        Context
    ).

register_for_crowd(Crowd, #{<<"name">> := _} = Data, Context) ->
    User = maps:get(<<"user">>, Data, undefined),
    Expiration = maps:get(<<"expiration">>, Data, undefined),
    % NOTE: Dirty fallback hack due to bad data types:
    Subject = maps:get(<<"subject">>, Data, maps:get(<<"keywords">>, Data, undefined)),
    case ensure_user_for_crowd(Crowd, User, Data, Context) of
        {ok, Id} when is_integer(Id) ->
            Result = link_crowd_and_subjects_to_user(Id, Crowd, Subject, Context),
            case Result of
                {ok, _} ->
                    add_participation_to_session(Crowd, Expiration, Context),
                    Result;
                _ ->
                    Result
            end;
        Error ->
            Error
    end;
register_for_crowd(_Crowd, _Data, _Context) ->
    {error, no_name}.


ensure_user_for_crowd(Crowd, undefined, Data, Context) ->
    Name = maps:get(<<"name">>, Data, undefined),
    Email = maps:get(<<"email">>, Data, undefined),
    ShowContact = maps:get(<<"show_contact">>, Data, undefined),
    Group = m_rsc:p(Crowd, content_group_id, Context),
    Props = [ {title, <<"Anoniem">>}
            , {content_group_id, Group}
            , {category, anonymous_participant}
            , {is_published, false}
            ],
    case m_rsc:insert(Props, Context) of
        {ok, User} ->
            % Create entry in temporary participant data table
            m_crowdparticipant:set(#{person => User, crowd => Crowd, name => Name, email => Email, show_contact => ShowContact}, Context),
            {ok, User};
        Error ->
            Error
    end;
ensure_user_for_crowd(_, User, _, _) when is_integer(User) ->
    User.

link_crowd_and_subjects_to_user(User, Crowd, Subjects, Context) ->
    % Link to Crowd
    Result = m_edge:insert(User, rsvp, Crowd, Context),
    case Result of
        {ok, _} ->
            % Link tags as subjects
            case Subjects of
                undefined ->
                    {ok, User};
                [Sub|Subs] ->
                    lists:foreach(
                      fun (S) ->
                              m_edge:insert(User, subject, S, Context)
                      end,
                      [Sub|Subs]
                     ),
                    {ok, User}
            end;
        Error ->
            Error
    end.

send_confirmation_mail_for_crowd(Crowd, Data, Context) when is_integer(Crowd) and is_map(Data) ->
    Name = maps:get(<<"name">>, Data, undefined),
    Email = maps:get(<<"email">>, Data, undefined),
    ShowContact = maps:get(<<"show_contact">>, Data, undefined),
    Subjects = maps:get(<<"keywords">>, Data, undefined),
    Expiration = m_crowd:expiration(Crowd, Context),
    Participant = crowdparticipant_signup:mk_participant_data(Name, Crowd, Email, ShowContact, Subjects, [], undefined, undefined, Expiration),
    case crowdparticipant_signup:is_valid(Participant, Context) of
        {false, Error} -> {error, Error};
        true ->
            UrlBase =
                z_dispatcher:abs_url(
                  z_dispatcher:url_for(crowd_mode, [{id, Crowd}, {mode, join}], Context),
                  Context
                 ),
            Link = crowdparticipant_signup:to_signup_url(UrlBase, Participant, Context),
            Vars =
                [ {crowd, Crowd }
                , {name, Name}
                , {email, Email}
                , {show_contact, ShowContact}
                , {subjects, Subjects}
                , {link, Link}
                ],
            z_email:send_render(Email, "email_confirmation.tpl", Vars, z_acl:sudo(Context))
    end.

add_participation_to_session(_Crowd, undefined, Context) ->
    Context;
add_participation_to_session(Crowd, Expiration, Context) ->
    Participations = participations_in_session(Context),
    Participation = {Crowd, Expiration},
    m_server_storage:store(daycrowds, [Participation|Participations], Context).

has_participation_in_session(Crowd, Context) ->
    Expiration = m_crowd:expiration(Crowd, Context),
    has_participation_in_session(Crowd, Expiration, Context).

has_participation_in_session(Crowd, {ExpirationDate, _}, Context) ->
    Participations = [{PartCrowd, Date}||{PartCrowd, {Date, _}} <- participations_in_session(Context)],
    Participation = {Crowd, ExpirationDate},
    lists:member(Participation, Participations);
has_participation_in_session(_Crowd, _, _Context) ->
    false.

participations_in_session(Context) ->
    case m_server_storage:lookup(daycrowds, Context) of
        {ok, Participations} when is_list(Participations) -> Participations;
        {error, no_session} -> [];
        {error, not_found} -> [];
        Other ->
            ?zError("Unexpected participation data in session: ~p~n", [Other], Context),
            []
    end.

% can view admin template with listing of all information
user_view_overview_crowdparticipant(Context) ->
    z_acl:is_allowed(use, mod_crowdparticipant, Context).

% can view admin template with info by user
user_view_crowdlink(Context) ->
    z_acl:is_allowed(use, mod_admin, Context).
