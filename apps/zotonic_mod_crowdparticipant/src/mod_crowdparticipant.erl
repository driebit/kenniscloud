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

-module(mod_crowdparticipant).

-mod_title("Crowd Participant").
-mod_description("Anonymous Crowd Participants for Meetup Crowd").
-mod_author("Driebit").
-mod_schema(1).

-export([
    observe_tick_1h/2,
    observe_admin_menu/3,
    observe_rsc_update/3,
    manage_schema/2,
    event/2,

    observe_crowdparticipant_name/2,
    observe_crowdparticipant_crowd/2,
    observe_crowdparticipant_email/2,
    observe_crowdparticipant_keyword/2,
    observe_crowdparticipant_custom_keyword/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

observe_tick_1h(tick_1h, Context) ->
    m_crowdparticipant:process_expiries_and_send_mails(Context).

observe_admin_menu(#admin_menu{}, Acc, _Context) ->
    [#menu_item{
        id = admin_crowdparticipant_overview,
        parent = admin_modules,
        label = "Crowd Participants",
        url = {crowdparticipant_admin_overview},
        visiblecheck = {acl, use, mod_admin}
       }
    | Acc ].

observe_rsc_update(#rsc_update{id = Id}, Acc, Context) ->
    case m_rsc:is_a(Id, event, Context) of
        true ->
            m_crowdparticipant:set_expiries_for_crowd(Id, Context),
            Acc;
        false ->
            Acc
    end.

event({postback, {delete_crowdparticipant, [{id, Id}]}, _TriggerId, _TargetId}, Context) ->
    m_crowdparticipant:delete(Id, Context),
    z_render:wire({reload, []}, Context);
event(#postback{message = {register, [{crowd, CrowdId}]}}, Context) ->
    true = z_context:is_session(Context), % bot-protection, request must come from a Zotonic page
    Email = z_context:get_q(<<"email">>, Context),
    SendResult =
        m_crowdparticipant:send_confirmation_mail_for_crowd(
            CrowdId,
            #{
                <<"name">> => z_context:get_q(<<"name">>, Context),
                <<"email">> => Email,
                <<"show_contact">> => z_context:get_q(<<"showContact">>, Context),
                <<"keywords">> => z_context:get_q(<<"keywords">>, Context)
            },
            Context
        ),
    case SendResult of
        {ok, _} ->
            z_render:growl(
              <<"Een mail om je aanmelding te bevestigen is verstuurd naar ", Email/binary>>,
              notice, true, % let the growl message stay
              Context
             );
        {error, Error} ->
            ?zError("Er ging helaas iets mis tijdens het registreren:~n~p~n", [Error], Context),
            Reason = string:replace(atom_to_list(Error), "_", " ", all),
            z_render:growl_error( "Er ging helaas iets mis tijdens het registreren: " ++
                                      Reason ++
                                      "\n\nVervers de pagina om het opnieuw te proberen.",
                                    Context)
    end;
event(#postback{message = {confirm, Args}}, Context) ->
    true = z_context:is_session(Context), % bot-protection, request must come from a Zotonic page
    {Registration, Validation} =
        case proplists:get_value(participant, Args, undefined) of
            undefined ->
                {undefined, {false, no_registration_data}};
            RegistrationEncoded ->
                R = crowdparticipant_signup:decode_participant(RegistrationEncoded, Context),
                V = crowdparticipant_signup:is_valid(R, Context),
                {R, V}
        end,
    case Validation of
        {false, Error} ->
            ?zError("Invalid incoming daycrowd registration: ~p", [Error], Context),
            Context1 = z_render:growl_error("Er ging helaas iets mis bij het aanmelden als deelnemer aan de crowd.", Context),
            z_render:wire({redirect, [{location, "/"}]}, Context1);
        true ->
            Data = crowdparticipant_signup:participant_data_to_map(Registration),
            case Data of
                #{<<"crowd">> := Crowd, <<"email">> := Email} ->
                    ExistingRegistration =
                        m_crowdparticipant:get_by_email_for_crowd(Crowd, Email, Context),
                    Confirmation =
                        case ExistingRegistration of
                            undefined ->
                                m_crowdparticipant:register_for_crowd(Crowd, Data, z_acl:sudo(Context));
                            _ ->
                                {error, email_already_registered}
                        end,
                    case Confirmation of
                        {ok, _} ->
                            Location = m_crowdlink:url(Crowd, Context),
                            z_render:wire({redirect, [{location, Location}]}, Context);
                        {error, email_already_registered} ->
                            ?zWarning("Email already registered, assuming genuine request; redirecting to Crowd ~p view", [Crowd], Context),
                            Location = m_crowdlink:url(Crowd, Context),
                            z_render:wire({redirect, [{location, Location}]}, Context);
                        {error, Error} ->
                            ?zError("Failed to register for Crowd ~p: ~p", [Crowd, Error], Context),
                            AbsUrl = m_rsc:page_url_abs(Crowd, Context),
                            Context1 = z_render:growl_error("Er ging helaas iets mis bij het aanmelden als deelnemer aan de crowd.", Context),
                            z_render:wire({redirect, [{location, AbsUrl}]}, Context1)
                    end;
                _ ->
                    ?zError("Missing crowd reference and/or email address in Crowd registration: " ++ Data, Context),
                    Context1 = z_render:growl_error("Er ging helaas iets mis bij het aanmelden als deelnemer aan de crowd.", Context),
                    z_render:wire({redirect, [{location, "/"}]}, Context1)
            end
    end.

manage_schema(_Version, Context) ->
    ok = m_crowdparticipant:install(Context).

%% Validators

observe_crowdparticipant_name({crowdparticipant_name, {postback, Id, Value, Args}}, Context) ->
    validator_crowdparticipant_name:validate(crowdparticipant_name, Id, Value, Args, Context).

observe_crowdparticipant_crowd({crowdparticipant_crowd, {postback, Id, Value, Args}}, Context) ->
    validator_crowdparticipant_crowd:validate(crowdparticipant_crowd, Id, Value, Args, Context).

observe_crowdparticipant_email({crowdparticipant_email, {postback, Id, Value, Args}}, Context) ->
    validator_crowdparticipant_email:validate(crowdparticipant_email, Id, Value, Args, Context).

observe_crowdparticipant_keyword({crowdparticipant_keyword, {postback, Id, Value, Args}}, Context) ->
    validator_crowdparticipant_keyword:validate(crowdparticipant_keyword, Id, Value, Args, Context).

observe_crowdparticipant_custom_keyword({crowdparticipant_custom_keyword, {postback, Id, Value, Args}}, Context) ->
    validator_crowdparticipant_custom_keyword:validate(crowdparticipant_custom_keyword, Id, Value, Args, Context).
