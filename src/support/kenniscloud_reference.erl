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

%% @doc Add reference prop to collaboration_group.
-module(kenniscloud_reference).

-export([
    event/2,
    get_opengraph_data/2,
    library_data_url_from_url/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

library_data_url_from_url(Url) ->
    case binary:split(Url, <<"bibliotheek.nl/catalogus/titel.">>) of
        [_,Rest] ->
            case binary:split(Rest, <<".">>) of
                [PPN|_] ->
                    <<"http://data.bibliotheek.nl/ggc/ppn/", PPN/binary>>;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.



-spec event(#submit{}, #context{}) -> ok.
event(#submit{message={add_reference, Args}}, Context) ->
    ReferenceUrl = z_html:sanitize_uri(z_context:get_q(<<"uri">>, Context)),
    SubjectId = proplists:get_value(subject_id, Args),
    TargetId = z_convert:to_binary(proplists:get_value(target_id, Args)),

    insert_opengraph_data(ReferenceUrl, SubjectId, Context),
    z_render:wire({form_reset, [{id, TargetId}]}, Context).

-spec insert_opengraph_data(string(), pos_integer(), #context{}) -> {ok, m_rsc:resource()} | {error, atom()}.
insert_opengraph_data(ReferenceUrl, SubjectId, Context) ->
    Props = #{
        <<"title">> => ReferenceUrl,
        <<"category">> => reference,
        <<"website">> => ReferenceUrl,
        <<"is_published">> => true,
        <<"uri">> => ReferenceUrl,
        <<"content_group_id">> => m_rsc:p_no_acl(SubjectId, <<"content_group_id">>, Context),
        <<"is_authoritative">> => false
    },
    {ok, Id} = case m_rsc:uri_lookup(ReferenceUrl, Context) of
        undefined -> m_rsc:insert(Props, Context);
        ExistingId -> m_rsc:update(ExistingId, Props, z_acl:sudo(Context))
    end,
    {ok, _EdgeId} = m_edge:insert(SubjectId, hasreference, Id, Context).

-spec get_opengraph_data(Url, Context) -> Metadata when
    Url :: string() | binary(),
    Context :: z:context(),
    Metadata :: m_rsc:props().
get_opengraph_data(Url, Context) ->
    Url1 = z_convert:to_list(Url),
    case z_url_metadata:fetch(Url1) of
        {ok, MD} ->
            ImageUrl = case z_url_metadata:p([<<"twitter:image:src">>, <<"twitter:image">>, <<"og:image">>], MD) of
                undefined -> undefined;
                Img -> z_html:sanitize_uri(Img)
            end,
            Meta = #{
                <<"og_title">> => z_url_metadata:p(title, MD),
                <<"og_description">> => z_url_metadata:p(description, MD),
                <<"og_image">> => ImageUrl
            },
            maps:filter(fun(_Key, Value) -> is_binary(Value) orelse Value =:= undefined end, Meta);
        {error, Reason} ->
            z:warning(
                "KennisCloud: error fetching opengraph data for Url '~s': ~p",
                [Url1, Reason],
                [],
                Context),
            #{}
    end.
