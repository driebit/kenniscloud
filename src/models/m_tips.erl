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

%% @author Driebit <tech@driebit.nl>
%% @copyright 2024
%% @doc API for tips

-module(m_tips).

-export([
    m_get/3,
    m_post/3,
    m_delete/3
]).

-behaviour(zotonic_model).
-include_lib("zotonic_core/include/zotonic.hrl").

% For API access via 'controller_api'.

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) ->
    zotonic_model:return().
% Path: /api/model/tips/get/for/<id>/
% Returns the tips related to the given resource.
m_get([ <<"for">>, PageIdBin | _Rest ], _Msg, Context) when is_binary(PageIdBin) ->
    PageId = z_convert:to_integer(PageIdBin),
    to_tips(PageId, Context);
% Unexpected query
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.


-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) ->
    {ok, term()} | ok | {error, term()}.
% Convert IDs to integers
m_post([PageId | Rest ], Payload, Context) when is_binary(PageId) ->
    m_post([z_convert:to_integer(PageId) | Rest], Payload, Context);

%% @doc API to add a tip to a page
% Path: /api/model/tips/post/<id>/
% Payload expected is a JSON object with the "uri" for a tip.
m_post([ PageId | _Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case maps:get(<<"uri">>, Payload, undefined) of
        undefined -> {error, <<"No 'uri' found in request body.">>};
        URI ->
            case z_sanitize:uri(URI) of
                undefined -> {error, <<"Invalid 'uri' in request body.">>};
                ValidUri ->
                    Props = #{
                        <<"category">> => reference,
                        <<"is_published">> => true,
                        <<"website">> => ValidUri
                    },
                    case m_rsc:insert(Props, Context) of
                        {ok, ReferenceId} ->
                            UserId = z_acl:user(Context),
                            {ok, _} = m_edge:insert(PageId, relation, ReferenceId, Context),
                            {ok, _} = m_edge:insert(ReferenceId, author, UserId, Context),
                            z:info("Created resource with id: ~p", [ReferenceId], [], Context),
                            {ok, ReferenceId};
                        Err ->
                            z:error("Something went wrong creating resource: ~p", [Err], [], Context),
                            Err
                    end
            end
    end;
% Unexpected payload
m_post(_Path, Payload, _Context) when not is_map(Payload) ->
    {error, invalid_payload};
% Unexpected query
m_post(_Path, _Payload, _Context) ->
    {error, unknown_path}.


-spec m_delete( list(), zotonic_model:opt_msg(), z:context() ) ->
    {ok, term()} | ok | {error, term()}.
%% @doc API to delete a tip
% Path: /api/model/tips/delete/<id>
m_delete([ Id | _Rest ], Msg, Context) ->
    case m_rsc:is_a(Id, reference, Context) of
        true -> m_rsc:m_delete([ Id ], Msg, Context);
        false -> {error, unknown_tip}
    end;
% Unexpected query
m_delete(_Path, _Msg, _Context) ->
    {error, unknown_path}.


% Internal functions

to_tips(PageId, Context) ->
    Rels = m_edge:objects(PageId, relation, Context),
    Tips = lists:map(fun(RefId) -> to_tip(RefId, Context) end, Rels),
    LoggedInUser = to_user_with_role(PageId, Context),
    {ok, #{loggedInUser => LoggedInUser, tips => Tips}}.

to_tip(ReferenceId, Context) ->
    #{
        id => ReferenceId,
        title => kenniscloud_utils:ensure_trans(m_rsc:p(ReferenceId, title, Context)),
        og_title => m_rsc:p(ReferenceId, og_title, Context),
        og_image => m_rsc:p(ReferenceId, og_image, Context),
        depiction_url =>
            case z_media_tag:url(ReferenceId, [{absolute_url, true}, {mediaclass, <<"image">>}], Context) of
                {ok, DepictionUrl} -> DepictionUrl;
                _ -> null
            end,
        uri => m_rsc:p(ReferenceId, uri, Context),
        author =>
            case m_edge:object(ReferenceId, author, 1, Context) of
                Y when is_integer(Y) ->
                    #{
                        authorId => Y,
                        title => kenniscloud_utils:ensure_trans(m_rsc:p(Y, title, Context))
                    };
                _ ->
                    null
            end
    }.

to_user_with_role(TipId, Context) ->
    case {z_acl:user(Context), m_edge:object(TipId, author, 1, Context)} of
        {UserId, AuthorId} when is_integer(UserId), is_integer(AuthorId) ->
            IsAuthor = UserId =:= AuthorId,
            GroupId = m_rsc:p(TipId, content_group_id, Context),
            IsCollabManager = lists:member(UserId, m_edge:objects(GroupId, hascollabmanager, Context)),
            case {IsAuthor, IsCollabManager} of
                {_, true} -> #{id => UserId, role => collab_manager};
                {true, _} -> #{id => UserId, role => page_author};
                _ -> #{id => UserId, role => member}
            end;
        _ -> null
    end.
