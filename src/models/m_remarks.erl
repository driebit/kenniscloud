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
%% @copyright 2025
%% @doc Model and API to interact with remarks

-module(m_remarks).

-export([
    m_get/3,
    m_post/3,

    contribution/2,
    mentioned/2,
    recipients/2
]).

-behaviour(zotonic_model).
-include_lib("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) ->
    zotonic_model:return().

% Syntax: m.remarks.for_contribution[id].latest
m_get([ <<"for_contribution">>, Contribution, <<"latest">> | Rest ], _Msg, Context) ->
    Args = [
        {cat, remark},
        {sort, "-rsc.modified"},
        {hasobject, [Contribution, about]},
        {pagelen, 1}
    ],
    Remarks = case m_search:search({query, Args}, Context) of
        #search_result{result = []} -> undefined;
        #search_result{result = [Result|_]} -> Result
    end,
    {ok, {Remarks, Rest}};
% Syntax: m.remarks[id].topic
m_get([ Resource, <<"topic">> | Rest ], _Msg, Context) ->
    Remark = case m_rsc:is_a(Resource, remark, Context) of
        true -> Resource;
        false -> undefined
    end,
    {ok, {contribution(Remark, Context), Rest}};

% For API access via 'controller_api'.

% Path: /api/model/remarks/get/for/<id>/
% Returns the remarks related to the given resource.
m_get([ <<"for">>, RIdBin | _Rest ], _Msg, Context) when is_binary(RIdBin) ->
    UserId = z_acl:user(Context),
    RId = z_convert:to_integer(RIdBin),
    z:info("Remarks API: get request for user ~p on res ~p", [UserId, RId], #{}, Context),
    case m_rsc:exists(RId, Context) of
        false -> {error, enoent};
        true ->
            case m_rsc:is_visible(RId, Context) of
                false -> {error, eacces};
                true ->
                    Group = m_rsc:p_no_acl(RId, content_group_id, Context),
                    User = get_current_user_data(Group, Context),
                    Remarks = get_remarks(RId, Group, Context),
                    OngoingTask = is_ongoing_task(RId, Context),
                    {ok, [{user, User}, {remarks, Remarks}, {group, Group}, {ongoing_task, OngoingTask}]}
            end
    end;

% Unexpected query
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.


-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) ->
    {ok, term()} | ok | {error, term()}.
% Convert IDs to integers
m_post([Action, RemarkId | _Rest ], Payload, Context) when is_binary(RemarkId) ->
    m_post([Action, z_convert:to_integer(RemarkId)], Payload, Context);

%% @doc API to add likes on remarks.
% Path: /api/model/remarks/post/like/<id>
% No payload is expected.
m_post([<<"like">>, RemarkId | _Rest ], _Payload, Context) ->
    UserId = z_acl:user(Context),
    z:info("Remarks API: like request for user ~p on res ~p", [UserId, RemarkId], #{}, Context),
    case m_rsc:exists(RemarkId, Context) of
        false -> {error, enoent};
        true -> m_edge:insert(UserId, like, RemarkId, Context)
    end;
%% @doc API to remove likes on remarks.
% Path: /api/model/remarks/post/unlike/<id>
% No payload is expected.
m_post([<<"unlike">>, RemarkId | _Rest ], _Payload, Context) ->
    UserId = z_acl:user(Context),
    z:info("Remarks API: unlike request for user ~p on res ~p", [UserId, RemarkId], #{}, Context),
    case m_rsc:exists(RemarkId, Context) of
        false -> {error, enoent};
        true -> m_edge:delete(UserId, like, RemarkId, Context)
    end;
%% @doc API to flag a remarks.
% Path: /api/model/remarks/post/flag/<id>
% No payload is expected.
m_post([<<"flag">>, RemarkId | _Rest ], _Payload, Context) ->
    UserId = z_acl:user(Context),
    z:info("Remarks API: flag request for user ~p on res ~p", [UserId, RemarkId], #{}, Context),
    case m_rsc:exists(RemarkId, Context) of
        false -> {error, enoent};
        true -> m_edge:insert(UserId, flag, RemarkId, Context)
    end;
%% @doc API to unflag a remarks.
% Path: /api/model/remarks/post/unflag/<id>
% No payload is expected.
m_post([<<"unflag">>, RemarkId | _Rest ], _Payload, Context) ->
    UserId = z_acl:user(Context),
    z:info("Remarks API: unflag request for user ~p on res ~p", [UserId, RemarkId], #{}, Context),
    case m_rsc:exists(RemarkId, Context) of
        false -> {error, enoent};
        true -> m_edge:delete(UserId, flag, RemarkId, Context)
    end;
%% @doc API to delete (unpublish) a remark.
% Path: /api/model/remarks/post/delete/<id>
% No payload is expected.
m_post([<<"delete">>, RemarkId | _Rest ], _Payload, Context) ->
    UserId = z_acl:user(Context),
    z:info("Remarks API: delete request for user ~p on res ~p", [UserId, RemarkId], #{}, Context),
    case m_rsc:exists(RemarkId, Context) of
        false -> {error, enoent};
        true -> m_rsc:update(RemarkId, #{<<"is_published">> => false}, Context)
    end;
%% @doc API to insert a new remark
% Path: /api/model/remarks/post/new/
% Payload expected is a JSON object with:
% - "title": the resource's 'title', optional
% - "body": the resource's 'body', optional
% - "content_group_id": the resource's content_group ID, optional
% - "about": the ID of the resource this remark refers to, optional
% - "mentions": a list of user IDs that this remark mentions, optional
% - "filename": a list of 'upload' records for images attached to this remark, optional
% - "for_task": whether this is submitted for the completion of a task, optional
m_post([<<"new">> | _Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    Title = maps:get(<<"title">>, Payload, <<>>),
    Body = maps:get(<<"body">>, Payload, <<>>),
    ContentGroupId = maps:get(<<"content_group_id">>, Payload, undefined),
    About = maps:get(<<"about">>, Payload, undefined),
    Mentions = z_parse_list:parse(maps:get(<<"mentions">>, Payload, <<"">>)),
    Attachments = maps:get(<<"filename">>, Payload, []),
    ForTask = z_convert:to_bool(maps:get(<<"for_task">>, Payload, false)),

    Props = #{
        <<"title">> => Title,
        <<"body">> => Body,
        <<"category">> => remark,
        <<"is_published">> => false,
        <<"for_task">> => ForTask,
        <<"content_group_id">> => ContentGroupId
    },

    UserId = z_acl:user(Context),
    z:info("Remarks API: new remark request for user ~p, props: ~p", [UserId, Props], #{}, Context),

    case m_rsc:insert(Props, Context) of
        {ok, RemarkId} ->
            m_edge:insert(RemarkId, about, About, Context),
            m_edge:replace(RemarkId, mention, Mentions, Context),
            upload_depiction(RemarkId, Attachments, Context),

            % note: we do this here and not on the insert so that when
            % kenniscloud_activity:maybe_register_activity is called, the edges
            % made/updated above already exists
            m_rsc:update(RemarkId, #{<<"is_published">> => true}, Context);
        Error ->
            Error
    end;
%% @doc API to update an existing remark
% Path: /api/model/remarks/post/update/<id>/
% Payload expected is a JSON object with:
% - "body": the resource's 'body', optional
% - "mentions": a list of user IDs that this remark mentions, optional
% - "filename": a list of 'upload' records for images attached to this remark, optional
% - "for_task": whether this is submitted for the completion of a task, optional
m_post([<<"update">>, RemarkId | _Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    UserId = z_acl:user(Context),
    z:info("Remarks API: update request for user ~p on res ~p", [UserId, RemarkId], #{}, Context),
    case maps:get(<<"body">>, Payload, undefined) of
        undefined -> ok;
        Body -> m_rsc:update(RemarkId, #{<<"body">> => Body}, Context)
    end,
    case maps:get(<<"mentions">>, Payload, undefined) of
        undefined -> ok;
        MentionsString -> m_edge:replace(RemarkId, mention, z_parse_list:parse(MentionsString), Context)
    end,
    case maps:get(<<"filename">>, Payload, undefined) of
        undefined -> ok;
        Attachments -> upload_depiction(RemarkId, Attachments, Context)
    end,
    case maps:get(<<"for_task">>, Payload, undefined) of
        undefined -> ok;
        ForTask -> m_rsc:update(RemarkId, #{<<"for_task">> => ForTask}, Context)
    end,
    ok;
%% @doc API to insert a reference URL for the given resource
% Path: /api/model/remarks/post/reference/<id>/
% Payload expected is the URL of the reference
m_post([<<"reference">>, RId | _Rest ], #{ payload := RefURL }, Context) when is_binary(RefURL) ->
    UserId = z_acl:user(Context),
    z:info("Remarks API: reference request for user ~p on res ~p with URL ~p", [UserId, RId, RefURL], #{}, Context),
    Props = #{
        <<"category">> => reference,
        <<"website">> => RefURL,
        <<"is_published">> => true
    },
    case m_rsc:insert(Props, Context) of
        {ok, ReferenceId} ->
            m_edge:insert(RId, about, ReferenceId, Context);
        Error ->
            Error
    end;
%% @doc API to delete all the objects of the 'about' predicate linked to the
% given resource.
% Path: /api/model/remarks/post/delete_abouts/<id>/
% No payload is expected.
m_post([<<"delete_abouts">>, RId | _Rest ], _Payload, Context) ->
    UserId = z_acl:user(Context),
    z:info("Remarks API: delete_abouts request for user ~p on res ~p", [UserId, RId], #{}, Context),
    lists:foreach(
        fun(ObjectId) ->
            m_rsc:delete(ObjectId, Context)
        end,
        m_edge:objects(RId, about, Context)
    );

% Unexpected query
m_post(_Path, _Payload, _Context) ->
    {error, unknown_path}.


%% @doc Get contribution that a remark is about.
-spec contribution(m_rsc:resource(), z:context()) -> m_rsc:resource().
contribution(Id, Context) ->
    case m_rsc:is_a(Id, contribution, Context) of
        true ->
            Id;
        false ->
            case m_edge:objects(Id, about, Context) of
                %% ignore self-referential edges
                [Id] -> Id;
                [Other] -> contribution(Other, Context);
                _ -> Id
            end
    end.

%% @doc Get a list of user ids that were mentioned in the remark.
-spec mentioned(m_rsc:resource(), z:context()) -> [m_rsc:resource()].
mentioned(Id, Context) ->
    m_edge:objects(Id, mention, Context).

%% @doc Get a list of user that should be notified of this remark
%% This includes the users 'mentioned' in the remark plus, if the remark is a
%% task submission, the creator of the task
recipients(Id, Context) ->
    Mentions = mentioned(Id, Context),
    Contribution = contribution(Id, Context),
    CreatorId = m_rsc:p_no_acl(Id, creator_id, Context),

    IsOnTask = m_rsc:is_a(Contribution, task, Context),
    IsTaskRemark = z_convert:to_bool(m_rsc:p_no_acl(Id, for_task, Context)),
    IsCreatorMentioned = lists:member(CreatorId, Mentions),
    case IsOnTask andalso IsTaskRemark andalso (not IsCreatorMentioned) of
        true -> [CreatorId | Mentions];
        false -> Mentions
    end.

% Internal functions

get_current_user_data(GroupId, Context) ->
    UserId = z_acl:user(Context),
    UserData = get_user_data(UserId, GroupId, Context),
    SignedIn = case UserId of
        undefined ->
            false;
        _ ->
            true
    end,
    [{user_signed_in, SignedIn} | UserData].

get_user_data(UserId, GroupId, Context) ->
    Title = kenniscloud_utils:prop_text(UserId, title, Context),
    SubTitle = kenniscloud_utils:prop_text(UserId, subtitle, Context),

    [
        {user_id, UserId},
        {user_title, Title},
        {user_subtitle, SubTitle},
        {user_avatar_url, get_avatar(UserId, Context)},
        {user_uri, kenniscloud_utils:prop_text(UserId, uri, Context)},
        {user_roles, m_kc_collab_group:roles_of(UserId, GroupId, Context)}
    ].

get_remarks(RscId, GroupId, Context) ->
    lists:filtermap(
        fun(Id) ->
            case m_rsc:is_a(Id, remark, Context) of
                true -> {true, get_remark(Id, GroupId, Context)};
                false -> false
            end
        end,
        m_edge:subjects(RscId, about, Context)
    ).

get_remark(Id, GroupId, Context) ->
    AuthorId = m_rsc:p_no_acl(Id, creator_id, Context),
    Date = m_rsc:p_no_acl(Id, created, Context),
    TimezoneOffset = list_to_binary(qdate:to_string("P", z_context:tz(Context), Date)),
    [
        {id, Id},
        {is_published, m_rsc:p(Id, is_published, false, Context)},
        {body, kenniscloud_utils:prop_text(Id, body, Context)},
        {date, Date},
        {timezone_offset, TimezoneOffset},
        {likes, get_likes(Id, Context)},
        {flags, get_flags(Id, Context)},
        {replies, get_remarks(Id, GroupId, Context)},
        {mentions, get_mentions(Id, Context)},
        {depiction, get_depiction(Id, Context)},
        {for_task, z_convert:to_bool(m_rsc:p_no_acl(Id, for_task, Context))},
        {author, get_user_data(AuthorId, GroupId, Context)}
    ].

get_mentions(RemarkId, Context) ->
    lists:map(
        fun(UserId) -> get_mention(UserId, Context) end,
        mentioned(RemarkId, Context)
    ).

get_mention(UserId, Context) ->
    [
        {username, kenniscloud_utils:prop_text(UserId, title, Context)},
        {user_id, UserId},
        {user_uri, kenniscloud_utils:prop_text(UserId, uri, Context)}
    ].

get_depiction(RemarkId, Context) ->
    case m_edge:objects(RemarkId, depiction, Context) of
        [DepictionId | _] ->
            case z_media_tag:url(DepictionId, [{absolute_url, true}], Context) of
                {ok, DepictionUrl} ->
                    [
                        {depiction_id, DepictionId},
                        {depiction_title, kenniscloud_utils:prop_text(DepictionId, title, Context)},
                        {depiction_url, DepictionUrl}
                    ];
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

get_likes(RemarkId, Context) ->
    lists:map(
        fun(PersonId) -> get_like(PersonId, RemarkId, Context) end,
        m_edge:subjects(RemarkId, like, Context)
    ).

get_like(PersonId, RemarkId, Context) ->
    [
        {username, kenniscloud_utils:prop_text(PersonId, title, Context)},
        {user_id, PersonId},
        {edge_id, m_edge:get_id(PersonId, like, RemarkId, Context)}
    ].

get_flags(RemarkId, Context) ->
    lists:map(
        fun(PersonId) -> get_flag(PersonId, RemarkId, Context) end,
        m_edge:subjects(RemarkId, flag, Context)
    ).

get_flag(PersonId, RemarkId, Context) ->
    [
        {username, kenniscloud_utils:prop_text(PersonId, title, Context)},
        {user_id, PersonId},
        {edge_id, m_edge:get_id(PersonId, flag, RemarkId, Context)}
    ].

get_avatar(undefined, Context) ->
    get_fallback_avatar(Context);
get_avatar(Id, Context) ->
    case m_edge:objects(Id, depiction, Context) of
        [] ->
            get_fallback_avatar(Context);
        _List ->
            AvatarId = m_media:depiction(Id, Context),
            case z_media_tag:url(AvatarId, avatar_url_options(), Context) of
                {ok, Url} -> Url;
                _ -> <<"">>
            end
    end.

get_fallback_avatar(Context) ->
    {ok, FallbackId} = m_rsc:name_to_id(custom_avatar_fallback, Context),
    case z_media_tag:url(FallbackId, avatar_url_options(), Context) of
        {ok, UrlFallback} -> UrlFallback;
        _ -> <<"">>
    end.

avatar_url_options() ->
    [
        {width, 64},
        {height, 64},
        {crop, center},
        {quality, 100},
        {absolute_url, true}
    ].

% Inserts a new media item from the first 'upload' record in the given list and
% makes it the only 'depiction' linked to the given resource ID.
% If no valid upload happens, all existing depictions are unlinked anyway.
upload_depiction(RscId, [Attachment | Rest], Context) when is_record(Attachment, upload) ->
    % Note: the image title is expected to be part of the attachment's 'upload' record
    case m_media:insert_file(Attachment, #{<<"is_dependent">> => true}, Context) of
        {ok, ImageId} -> m_edge:replace(RscId, depiction, [ImageId], Context);
        Error ->
            z:warning(
                "Remarks API: could not insert image ~p for #~p: ~p",
                [Attachment, RscId, Error],
                #{},
                Context
            ),
            upload_depiction(RscId, Rest, Context)
    end;
upload_depiction(RscId, [_Other | Rest], Context) ->
    upload_depiction(RscId, Rest, Context);
upload_depiction(RscId, _, Context) ->
    m_edge:replace(RscId, depiction, [], Context).

is_ongoing_task(Id, Context) ->
    m_rsc:is_a(Id, task, Context) andalso
        (not m_rsc:p(Id, is_completed, false, Context)).
