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
    event/2,
    update_keywords/1,
    update_keywords/2,

    import_keywords/2,

    fetch_keywords/1,
    fetch_keywords/2,
    fetch_suggestions/2,

    fetch/4,

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

event(#postback{message=update_keywords}, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            update_keywords(Context),
            z_render:growl("AZB: library keyword refresh started", Context);
        _ ->
            z_render:growl_error("Only admins are allowed to start this", Context)
    end.

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
        <<"kenniscloud_azb:import_keywords:", (z_string:to_slug(Term))/binary>>,
        [Term, Context],
        Context
    ).


import_keywords(Term, Context) when is_binary(Term) orelse Term =:= undefined ->
    ?zInfo("AZB: fetching keyword from: ~p", [Term], Context),
    import_keywords(fetch_keywords(Term, Context), Context);
import_keywords(KeywordList, Context) when is_list(KeywordList) ->
    lists:foreach(
        fun(Keyword) -> import_keyword(Keyword, Context) end,
        KeywordList
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
    XmlMap = fetch(Term, [], true, Context),
    extract_keywords(XmlMap, Context).


% Fetch suggestions from the keywords (ids) using a search query with facets, see:
% https://azb.zbkb.nl/demo/documentation/search.html
% Return a list of suggestions, each represented by a map containing:
% - title
% - ppn
% - uri
% - creator
% - genre
% - date
fetch_suggestions(KeywordIds, Context) ->
    KeywordKeys = lists:map(
        fun(KeywordId) -> m_rsc:p(KeywordId, <<"keyword_id">>, Context) end,
        KeywordIds
    ),
    XmlMap = fetch(undefined, KeywordKeys, false, Context),
    % Since we receive keywords while looking for suggestions too, we import them:
    FoundKeywords = extract_keywords(XmlMap, Context),
    import_keywords(FoundKeywords, Context),
    % before returning all found records as suggestions:
    extract_suggestions(XmlMap, Context).


% Fetch results from the API, optionally starting from a term
% https://azb.zbkb.nl/demo/documentation/search.html
% Return a list of tuples with ID and label of each valid keyword found.
fetch(Term, KeywordKeys, IdsOnly, Context) ->
    case auth_bearer(Context) of
        undefined ->
            [];
        AuthBearer ->
            Options = [{accept, <<"text/xml">>}, {authorization, AuthBearer}],
            BaseQueryArgs = [
                % query for everything ('*') or for NBC terms:
                {<<"query">>,
                    if
                        is_binary(Term) -> <<"nbc:all:", (z_string:to_slug(Term))/binary>>;
                        Term =:= undefined -> <<"*">>
                    end
                },
                % get the maximum amount of results
                {<<"size">>, <<"50">>},
                {<<"from">>, <<"0">>},
                % only get the identifiers of the resulting titles
                {<<"identifiersOnly">>, z_convert:to_binary(IdsOnly)},
                % only get the keyword facet, with the maximum size allowed
                {<<"facet">>, <<"nbc:subjectNbdtrefwoorden_key">>},
                {<<"facetSize">>, <<"100">>}
            ],
            FilterQueryArgs = lists:filtermap(
                fun
                    (<<"subject~nbdtrefwoorden~", _Rest/binary>> = KeywordKey) ->
                        {
                            true,
                            {
                                <<"filter">>,
                                <<"nbc:subjectNbdtrefwoorden_key:", KeywordKey/binary>>
                            }
                        };
                    (_KeywordKey) ->
                        false
                end,
                KeywordKeys
            ),
            QueryArgs = BaseQueryArgs ++ FilterQueryArgs,
            case z_fetch:fetch(get, ?SEARCH_URL, QueryArgs, Options, Context) of
                {ok, {_FinalUrl, _RespHeaders, _ContentLength, Content}} ->
                    case z_html_parse:parse_to_map(Content, #{mode => xml}) of
                        {ok, XmlMap} ->
                            XmlMap;
                        {error, Error} ->
                            ?zError(
                                "AZB: invalid results XML: ~p",
                                [Error],
                                Context
                            ),
                            #{}
                    end;
                {error, Error} ->
                    ?zError(
                        "AZB: couldn't fetch results: ~p",
                        [Error],
                        Context
                    ),
                    #{}
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


extract_suggestions(#{
    <<"nbc:searchResult">> := [#{<<"nbc:results">> := [#{<<"nbc:result">> := SearchResults}]}]
}, _Context) when is_list(SearchResults) ->
    lists:filtermap(fun extract_suggestion/1, SearchResults);
extract_suggestions(Response, Context) ->
    ?zError(
        "AZB: unexpected results XML: ~p",
        [Response],
        Context
    ),
    [].

extract_suggestion(#{<<"nbc:record">> := [RecordMap]}) when is_map(RecordMap) ->
    extract_suggestion(RecordMap);
extract_suggestion(#{<<"@attributes">> := Attrs} = RecordMap) when is_map(RecordMap) ->
    case proplists:get_value(<<"nbc:identifier">>, Attrs, undefined) of
        <<"PPN:", Ppn/binary>> ->
            Result = #{
                <<"ppn">> => Ppn,
                <<"uri">> => <<"https://www.bibliotheek.nl/catalogus/titel.", Ppn/binary,".html">>,
                <<"title">> => extract_record_title(RecordMap),
                <<"creator">> => extract_record_creator(RecordMap),
                <<"genre">> => extract_record_genre(RecordMap),
                <<"date">> => extract_record_date(RecordMap)
            },
            {true, Result};
        _ ->
            false
    end;
extract_suggestion(_) ->
    false.

extract_record_title(RecordMap) ->
    case maps:get(<<"nbc:title">>, RecordMap, undefined) of
        [#{<<"nbc:displayTitle">> := [Title]} | _] ->
            Title;
        _ ->
            undefined
    end.

extract_record_creator(RecordMap) ->
    case maps:get(<<"nbc:creator">>, RecordMap, undefined) of
        [#{<<"nbc:nameProfile2">> := [#{<<"value">> := [Name]}]} | _] ->
            Name;
        _ ->
            undefined
    end.

extract_record_genre(RecordMap) ->
    case maps:get(<<"nbc:subject">>, RecordMap, undefined) of
        SubjectsList when is_list(SubjectsList) ->
            GenreList = lists:filtermap(
                fun
                    (#{<<"@attributes">> := Attrs, <<"value">> := [Value]}) ->
                        case proplists:get_value(<<"nbc:domain">>, Attrs, undefined) of
                            <<"subject~nbdgenre">> -> {true, Value};
                            <<"subject~nbchoofdcategorie">> -> {true, Value};
                            _ -> false
                        end;
                    (_) ->
                        false
                end,
                SubjectsList
            ),
            case GenreList of
                [Genre | _] when is_binary(Genre) -> Genre;
                _ -> undefined
            end;
        _ ->
            undefined
    end.

extract_record_date(RecordMap) ->
    case maps:get(<<"nbc:publication">>, RecordMap, undefined) of
        [#{<<"nbc:year">> := [Year]} | _] ->
            Year;
        _ ->
            undefined
    end.

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
