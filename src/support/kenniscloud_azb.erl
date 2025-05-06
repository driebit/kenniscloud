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

%% @doc Support and coupling with the AZB API
%% Online documentation: https://azb.zbkb.nl/demo/documentation/_Sidebar.html

-module(kenniscloud_azb).

-export([
    update_keywords/1,
    update_keywords/2,

    import_keywords/2,

    fetch_keywords/1,
    fetch_keywords/2,

    aid/1,
    apikey/1,
    sid/1,
    token/1,
    refresh_token/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(AUTH_URL, "https://azb.zbkb.nl/authorize").
-define(SEARCH_URL, "https://v1.azb.zbkb.nl/title/search").

%% IMPORTING

% Schedules jobs to update the keywords imported from AZB.
% This will trigger an import without term and an import for each existing
% keyword, using its label as search term.
update_keywords(Context) ->
    schedule_import_keywords(undefined, Context),
    m_category:foreach(library_keyword, fun update_keywords/2, Context).

update_keywords(RscId, Context) ->
    case m_rsc:p(RscId, <<"keyword_label">>, Context) of
        Value when is_binary(Value) -> schedule_import_keywords(Value, Context);
        _ -> ok
    end.



schedule_import_keywords(undefined, Context) ->
    z_pivot_rsc:insert_task(
        kenniscloud_azb,
        import_keywords,
        <<"kenniscloud_azb:import_keywords">>,
        [undefined, Context],
        Context
    );
schedule_import_keywords(Term, Context) when is_binary(Term) ->
    z_pivot_rsc:insert_task(
        kenniscloud_azb,
        import_keywords,
        <<"kenniscloud_azb:import_keywords:", Term/binary>>,
        [Term, Context],
        Context
    ).


import_keywords(Term, Context) ->
    ?zInfo("AZB: fetching keyword from: ~p", [Term], Context),
    lists:foreach(
        fun(Keyword) -> import_keyword(Keyword, Context) end,
        fetch_keywords(Term, Context)
    ).

import_keyword({NbcKey, NbcLabel}, Context) when is_binary(NbcKey) andalso is_binary(NbcLabel) ->
    UniqueName = z_string:to_name(NbcKey),
    case m_rsc:rid(UniqueName, Context) of
        undefined ->
            Props = #{
                <<"title">> => NbcLabel,
                <<"is_published">> => true,
                <<"keyword_id">> => NbcKey,
                <<"keyword_label">> => NbcLabel,
                <<"category">> => library_keyword,
                <<"name">> => UniqueName
            },
            case m_rsc:insert(Props, z_acl:sudo(Context)) of
                {ok, _RscId} ->
                    ok;
                {error, Error} ->
                    ?zError(
                        "AZB: failed to insert new keyword (~p) reason: ~p",
                        [Props, Error],
                        Context
                    ),
                    ok
            end;
        _ ->
            ok
    end;
import_keyword(Value, Context) ->
    ?zError(
        "AZB: cannot import keyword: ~p",
        [Value],
        Context
    ),
    ok.

%% SEARCHING

% Like 'fetch_keywords' but without looking for a specific term.
fetch_keywords(Context) ->
    fetch_keywords(undefined, Context).

% Fetch keywords from the given term using a search query with facets, see:
% https://azb.zbkb.nl/demo/documentation/search.html
% Return a list of tuples with ID and label of each valid keyword found.
fetch_keywords(Term, Context) ->
    case auth_bearer(Context) of
        undefined ->
            [];
        AuthBearer ->
            Options = [{accept, <<"text/xml">>}, {authorization, AuthBearer}],
            QueryArgs = [
                % query for everything ('*') or for NBC terms:
                {<<"query">>,
                    if
                        is_binary(Term) -> <<"nbc:all:", Term/binary>>;
                        Term =:= undefined -> <<"*">>
                    end
                },
                % get the maximum amount of results
                {<<"size">>, <<"50">>},
                {<<"from">>, <<"0">>},
                % only get the identifiers of the resulting titles
                {<<"identifiersOnly">>, <<"true">>},
                % only get the keyword facet, with the maximum size allowed
                {<<"facet">>, <<"nbc:subjectNbdtrefwoorden_key">>},
                {<<"facetSize">>, <<"100">>}
            ],
            case z_fetch:fetch(get, ?SEARCH_URL, QueryArgs, Options, Context) of
                {ok, {_FinalUrl, _RespHeaders, _ContentLength, Content}} ->
                    case z_html_parse:parse_to_map(Content, #{mode => xml}) of
                        {ok, XmlMap} ->
                            extract_keywords(XmlMap, Context);
                        {error, Error} ->
                            ?zError(
                                "AZB: invalid results XML: ~p",
                                [Error],
                                Context
                            ),
                            []
                    end;
                {error, Error} ->
                    ?zError(
                        "AZB: couldn't fetch results: ~p",
                        [Error],
                        Context
                    ),
                    []
            end
    end.

extract_keywords(#{
    <<"nbc:searchResult">> := [#{<<"nbc:facets">> := [#{<<"nbc:facet">> := FacetResults}]}]
}, _Context) when is_list(FacetResults) ->
    lists:filtermap(fun extract_keyword/1, FacetResults);
extract_keywords(Response, Context) ->
    ?zError(
        "AZB: unexpected results XML: ~p",
        [Response],
        Context
    ),
    [].

extract_keyword(#{
    <<"nbc:domain">> := [<<"subject~nbdtrefwoorden">>],
    <<"nbc:key">> := [NbcKey],
    <<"nbc:label">> := [NbcLabel]
}) when is_binary(NbcKey) andalso is_binary(NbcLabel) ->
    {true, {NbcKey, NbcLabel}};
extract_keyword(_) ->
    false.


%% CONFIG & AUTH

aid(Context) ->
    case m_config:get_value(azb, aid, Context) of
        undefined ->
            ?zError(
                "AZB: missing required configuration 'aid'",
                Context
            ),
            undefined;
        Value ->
            z_convert:to_binary(Value)
    end.

apikey(Context) ->
    case m_config:get_value(azb, apikey, Context) of
        undefined ->
            ?zError(
                "AZB: missing required configuration 'apikey'",
                Context
            ),
            undefined;
        Value ->
            z_convert:to_binary(Value)
    end.

sid(Context) ->
    case m_config:get_value(azb, sid, Context) of
        undefined ->
            {NewSid, _NewToken} = refresh_token(Context),
            NewSid;
        Value ->
            z_convert:to_binary(Value)
    end.

token(Context) ->
    case m_config:get_value(azb, token, Context) of
        undefined ->
            {_NewSid, NewToken} = refresh_token(Context),
            NewToken;
        Value ->
            z_convert:to_binary(Value)
    end.

auth_bearer(Context) ->
    auth_bearer(aid(Context), sid(Context), token(Context)).

auth_bearer(Aid, Sid, Token) when is_binary(Aid) andalso is_binary(Sid) andalso is_binary(Token) ->
    <<"Bearer ", Aid/binary, ":", Sid/binary, ":", Token/binary>>;
auth_bearer(_, _, _) ->
    undefined.

% Retrieve the current authorization token for the API, see:
% https://azb.zbkb.nl/demo/documentation/autorisatie.html
% Returns a tuple with 'sid' and 'token', that also get saved in the config.
% Note: as per documentation, this should be run daily.
refresh_token(Context) ->
    case {aid(Context), apikey(Context)} of
        {undefined, _} -> {undefined, undefined};
        {_, undefined} -> {undefined, undefined};
        {Aid, ApiKey} ->
            Auth = <<"Basic ", Aid/binary, ":", ApiKey/binary>>,
            case z_fetch:fetch_json(?AUTH_URL, [{authorization, Auth}], Context) of
                {ok, #{<<"sid">> := Sid, <<"token">> := Token}} ->
                    SidBin = z_convert:to_binary(Sid),
                    TokenBin = z_convert:to_binary(Token),
                    SudoContext = z_acl:sudo(Context),

                    m_config:set_value(azb, sid, SidBin, SudoContext),
                    m_config:set_value(azb, token, TokenBin, SudoContext),
                    {SidBin, TokenBin};
                {ok, Response} ->
                    ?zError(
                        "AZB: unexpected auth token response: ~p",
                        [Response],
                        Context
                    ),
                    {undefined, undefined};
                {error, Error} ->
                    ?zError(
                        "AZB: couldn't fetch auth token: ~p",
                        [Error],
                        Context
                    ),
                    {undefined, undefined}
            end
    end.
