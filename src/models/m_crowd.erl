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

-module(m_crowd).

-export([
    m_get/3,

    is_daycrowd/2,
    category/2,
    expiration/2
]).

-behaviour(zotonic_model).
-include("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> {ok, { term(), list() }} | {error, term()}.

% Syntax: m.crowd[id].is_daycrowd
m_get([ Crowd, <<"is_daycrowd">> | Rest ], _Msg, Context) ->
    {ok, {is_daycrowd(Crowd, Context), Rest}};

% For API access via 'controller_api'.

% Path: /api/model/crowd/get/data/<id>/<crowdlink>
m_get([ <<"data">>, RIdBin, Crowd | _Rest ], _Msg, Context) when is_binary(RIdBin) andalso is_binary(Crowd) ->
    {ok, get_crowd_data(RIdBin, Crowd, Context)};
% Path: /api/model/crowd/get/data/<id>/
m_get([ <<"data">>, RIdBin | _Rest ], _Msg, Context) when is_binary(RIdBin) ->
    {ok, get_crowd_data(RIdBin, undefined, Context)};

% Unexpected query
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

is_daycrowd(CrowdId, Context) ->
    m_rsc:is_a(CrowdId, daycrowdevent, Context).

% TODO - rename as category would be confusing with Zotonic categories
category(CrowdId, Context) ->
    case is_daycrowd(CrowdId, Context) of
        true -> daycrowd;
        false -> crowd
    end.

expiration(CrowdId, Context) ->
    m_rsc:p_no_acl(CrowdId, date_end, Context).


get_crowd_data(RIdBin, CrowdLink, Context) ->
    EventId = z_convert:to_integer(RIdBin),
    IsElevated = m_crowdlink:is_valid(EventId, CrowdLink, Context),

    #{
        id => EventId,
        title => kenniscloud_utils:ensure_trans(m_rsc:p(EventId, title, Context)),
        tags => encode_tags(EventId, Context),
        summary => kenniscloud_utils:ensure_trans(m_rsc:p(EventId, summary, Context)),
        categories => m_rsc:is_a(EventId, Context),
        participants => encode_participants(EventId, IsElevated, Context),
        is_elevated => IsElevated
    }.


encode_tags(RscId, Context) ->
    lists:map(
        fun(TagId) ->
            #{
                id => TagId,
                title => kenniscloud_utils:ensure_trans(m_rsc:p(TagId, title, Context))
            }
        end,
        m_edge:objects(RscId, subject, Context)
    ).


encode_avatar(RscId, Context) ->
    % Like 'm_rsc:p_no_acl(RscId, <<"image_url_abs">>, Context)', but returns an
    % error when there is no image associated with the given resource.
    case z_media_tag:url(RscId, [{absolute_url, true}, {mediaclass, <<"image">>}], Context) of
        {ok, URL} when is_binary(URL) -> URL;
        _ -> undefined
    end.

encode_participants(EventId, IsElevated, Context) ->
    lists:map(
        fun(ParticipantId) ->
            encode_participant(EventId, IsElevated, ParticipantId, Context)
        end,
        m_edge:subjects(EventId, rsvp, Context)
    ).

encode_participant(EventId, IsElevated, ParticipantId, Context) ->
    case m_rsc:is_a(ParticipantId, anonymous_participant, Context) of
        true -> encode_anonymous(EventId, IsElevated, ParticipantId, Context);
        false ->
            #{
                id => ParticipantId,
                title => kenniscloud_utils:ensure_trans(m_rsc:p(ParticipantId, title, Context)),
                tags => encode_tags(ParticipantId, Context),
                avatar => encode_avatar(ParticipantId, Context),
                summary => kenniscloud_utils:ensure_trans(m_rsc:p(ParticipantId, summary, Context)),
                email => m_rsc:p(ParticipantId, email, Context),
                categories => m_rsc:is_a(EventId, Context)
            }
    end.

encode_anonymous(EventId, true, ParticipantId, Context) ->
    case m_crowdparticipant:get(ParticipantId, Context) of
        #{ crowd_id := EventId, name := Name, email := Email, show_contact := ShowContact } ->

            ShownEmail = case m_crowd:category(EventId, Context) of
                daycrowd when not ShowContact -> undefined;
                _Else -> Email
            end,
            #{
                id => ParticipantId,
                title => kenniscloud_utils:ensure_trans(Name),
                tags => encode_tags(ParticipantId, Context),
                avatar => encode_avatar(ParticipantId, Context),
                summary => kenniscloud_utils:ensure_trans(m_rsc:p(ParticipantId, summary, Context)),
                email => ShownEmail,
                categories => m_rsc:is_a(EventId, Context)
            };
        _ ->
            encode_anonymous(EventId, false, ParticipantId, Context)
    end;
encode_anonymous(_EventId, false, ParticipantId, Context) ->
    #{
        id => ParticipantId,
        tags => encode_tags(ParticipantId, Context)
    }.

