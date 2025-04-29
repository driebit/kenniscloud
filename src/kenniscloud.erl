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

-module(kenniscloud).
-author("Driebit <tech@driebit.nl>").

-mod_title("Kenniscloud").
-mod_description("").
-mod_prio(10).
-mod_depends([mod_crowdlink, mod_crowdparticipant, mod_driebit_activity2, mod_driebit_base, mod_driebit_edit, mod_image_edit]).
-mod_schema(18).

-include_lib("zotonic_core/include/zotonic.hrl").

-include_lib("zotonic_mod_driebit_activity2/src/include/driebit_activity2.hrl").

-record(rdf_value, {
    value :: term(),
    language = undefined :: undefined | binary(),
    type = undefined :: undefined | ginger_uri:uri()
}).

-record(rdf_resource, {
    id :: ginger_uri:uri(),
    triples = [] :: [m_rdf:triple()]
}).

-record(triple, {
    %% DEPRECATED: type property is deprecated. Construct an object = #rdf_value{value = ...}
    %% instead.
    type = literal :: resource | literal,

    subject :: undefined | binary(),
    subject_props = [] :: proplists:proplist(),
    predicate :: m_rdf:predicate(),
    object :: ginger_uri:uri() | #rdf_value{} | #rdf_resource{},
    object_props = [] :: proplists:proplist()
}).

-record(rsc_entity_text, {
    id :: term(),
    language :: atom()
}).

%% @doc Notification to convert a Zotonic resource to an RDF resource
-record(rsc_to_rdf, {
    id :: m_rsc:resource()
}).

-export([
    manage_schema/2,
    manage_data/2,
    generate_hdt_dataset/2,
    event/2,

    observe_acl_is_allowed/2,
    observe_acl_is_allowed_prop/2,

    observe_rsc_insert/3,
    observe_rsc_entity_text/3,
    observe_rsc_update/3,
    observe_rsc_update_done/2,
    observe_custom_pivot/2,
    observe_logon_ready_page/2,
    observe_signup_done/2,
    observe_signup_form_fields/3,
    observe_signup_confirm/2,
    observe_signup_confirm_redirect/2,
    observe_search_query_term/2,
    observe_driebit_activity2_inserted/2,
    observe_edge_insert/2,
    observe_edge_delete/2,
    observe_rsc_to_rdf/3,
    observe_tick_1h/2,
    observe_tick_24h/2,
    observe_validate_subjects/2,
    init/1
]).


init(Context) ->
    z_pivot_rsc:define_custom_pivot(
      kenniscloud_users,
      [{has_depiction, "boolean"}],
      Context),
    m_config:set_value(mod_acl_user_groups, collab_group_link, <<"member">>, Context),
    m_config:set_value(mod_acl_user_groups, collab_group_update, <<"manager">>, Context),
    m_config:set_value(site, maptiler_key, <<"">>, Context),
    m_config:set_value(mod_geomap, zoomlevel, <<"8">>, Context),
    m_config:set_value(mod_geomap, location_lat, <<"52.005554">>, Context),
    m_config:set_value(mod_geomap, location_lng, <<"5.197394">>, Context),
    case m_site:environment(Context) of
        development -> ok;
        _ -> m_config:set_value(site, protocol, <<"https">>, Context)
    end,
    ok.

manage_schema(Version, Context) ->
    kenniscloud_schema:manage_schema(Version, Context).

manage_data(Version, Context) ->
    kenniscloud_schema:manage_data(Version, Context).

generate_hdt_dataset(QueryId, Context) ->
    %% Serialize data
    IoData = lists:map(
               fun(Id) ->
                       R = m_rdf:to_triples(Id, Context),
                       ginger_turtle_kenniscloud:serialize(R)
               end,
               z_search:query_([{query_id, QueryId}], Context)
              ),
    %% Write to file
    Ttl = io_lib:format("/tmp/kenniscloud_~s.ttl", [QueryId]),
    ok = file:write_file(Ttl, IoData, [write]),
    %% Convert to HDT
    Hdt = io_lib:format("/tmp/kenniscloud_~s.hdt", [QueryId]),
    Cmd1 = io_lib:format("rdf2hdt ~s ~s", [Ttl, Hdt]),
    os:cmd(Cmd1),
    %% Move file to "files"
    SiteDir = z_path:site_dir(Context),
    %% 1) Create dir
    os:cmd(io_lib:format("mkdir ~s/files/hdt", [SiteDir])),
    %% 2) Move file
    os:cmd(io_lib:format("mv ~s ~s/files/hdt/~s.hdt", [Hdt, SiteDir, QueryId])),
    %% Done
    ok.

event(#postback{message={contribution_to_task, [{id, ContributionId}]}}, Context) ->
    case m_rsc:update(ContributionId, #{<<"category_id">> => task}, Context) of
        {ok, ContributionId} ->
            z_render:wire({reload, []}, Context);
        _ ->
            z_render:growl_error(?__("Something went wrong. Sorry.", Context), Context)
    end;

event(#postback{message={complete_task, [{id, ContributionId}]}}, Context) ->
    case m_rsc:update(ContributionId, #{<<"is_completed">> => true}, Context) of
        {ok, ContributionId} ->
            z_render:wire({reload, []}, Context);
        _ ->
            z_render:growl_error(?__("Something went wrong. Sorry.", Context), Context)
    end;

event(#postback{message={join, [{target, TargetId}]}}, Context) ->
    User = z_acl:user(Context),
    true = m_rsc:is_a(TargetId, acl_collaboration_group, Context),
    m_edge:insert(TargetId, hascollabmember, User, z_acl:sudo(Context)),
    Context;
event(#postback{message={leave, [{target, TargetId}]}}, Context) ->
    User = z_acl:user(Context),
    m_edge:delete(TargetId, hascollabmember, User, z_acl:sudo(Context)),
    Context;
event(#postback{message={link_rsvp,
        [{subject_id, Subject}, {predicate, <<"rsvp">>}, {object_id, Object} | _AndActionEtc]
    }}, Context) ->
    SubjectTopics = m_rsc:o(Subject, subject, Context),
    ObjectTopics = m_rsc:o(Object, subject, Context),
    case sets:is_disjoint(sets:from_list(SubjectTopics), sets:from_list(ObjectTopics)) of
        true ->
            z_render:growl("Kies minimaal 1 thema.", Context);
        false ->
            m_edge:insert(Subject, rsvp, Object, Context),
            Context1 = z_render:growl([
                                       <<"Aangemeld voor \"">>,
                                       m_rsc:p(Object, title, Context),
                                       <<"\"">>
                                      ], Context),
            Location = z_dispatcher:url_for(crowd, [{id, Object}], Context1),
            z_render:wire({redirect, [{location, Location}]}, Context1)
    end;
event(#postback{message={unsubscribe, [{mailing, <<"weekly">>}]}}, Context) ->
    kenniscloud_weekly:unsubscribe(Context),
    Context;
event(#postback{message={unsubscribe, [{mailing, <<"notifications">>}]}}, Context) ->
    kenniscloud_notifications:unsubscribe(Context),
    Context;
event(#postback{message={admin_send_weekly_preview, _Args}}, Context) ->
    User = z_acl:user(Context),
    Message = case kenniscloud_weekly:send(User, Context) of
        noop -> "Er zijn geen recente bijdragen om een email mee op te stellen";
        ok -> "De voorbeeld email is naar je verstuurd"
    end,
    z_render:growl(Message, Context);
event(#submit{message={emailpreferences, _Args}}, Context) ->
    Updates = #{
        <<"receive_weekly_update_mail">> =>
            z_convert:to_bool(z_context:get_q(<<"receive_weekly_update_mail">>, Context)),
        <<"receive_notification_mail">> =>
            z_convert:to_bool(z_context:get_q(<<"receive_notification_mail">>, Context))
    },
    m_rsc_update:update(z_acl:user(Context), Updates, Context),
    Context;

event(#submit{message={new_group, Args}}, Context) ->
    Redirect = maps:get(<<"redirect">>, Args, undefined),
    SubjectId = z_convert:to_integer(maps:get(<<"subject_id">>, Args, undefined)),
    Predicate = maps:get(<<"predicate">>, Args, undefined),
    Callback = maps:get(<<"callback">>, Args, undefined),
    Actions = maps:get(<<"actions">>, Args, []),
    Objects = maps:get(<<"objects">>, Args, []),

    Title = z_context:get_q(<<"new_rsc_title">>, Context),
    CategoryId = z_context:get_q(<<"category_id">>, Context),
    CgId = z_context:get_q(<<"content_group_id">>, Context),
    IsPublished = z_context:get_q(<<"is_published">>, Context),

    Props = #{
        <<"title">> => #trans{tr = [{nl, Title}]},
        <<"language">> => <<"nl">>,
        <<"is_published">> => IsPublished,
        <<"category_id">> => CategoryId
    },

    {ok, Id} = m_rsc_update:insert(Props, Context),

    SudoContext = z_acl:sudo(Context),
    Name = "cg_" ++ integer_to_list(Id) ++ "_" ++ z_string:to_slug(Title),
    case CgId of
        undefined ->
            none;
        <<"own">> ->
            m_rsc_update:update(Id, #{<<"content_group_id">> => Id, <<"name">> => Name}, SudoContext);
        Value ->
            m_rsc_update:update(Id, #{<<"content_group_id">> => Value}, SudoContext)
    end,

    % add an edge from the subject to this new resource
    mod_admin:do_link(SubjectId, Predicate, Id, Callback, Context),

    %% add outgoing edges from this new rsc to the given resources (id / name, predicate pairs)
    [m_edge:insert(Id, Pred, m_rsc:rid(Object, Context), Context) || [Object, Pred] <- Objects],

    z_render:wire([{dialog_close, []} | Actions], Context),
    case Redirect of
        undefined -> none;
        Dispatch ->
            Location = z_dispatcher:url_for(Dispatch, [{id, Id}], Context),
            z_render:wire({redirect, [{location, Location}]}, Context)
    end;

event(#submit{message={sudo_delete_profile, Args}}, Context0) ->
    SudoContext = z_acl:sudo(Context0),
    Redirect = proplists:get_value(redirect, Args, undefined),
    case z_acl:user(Context0) of
        UserId when is_integer(UserId) ->
            % Delete properties
            Props = #{
                <<"title">> => #trans{tr = [{nl,"Uitgeschreven lid"}]},
                <<"subtitle">> => undefined,
                <<"summary">> => undefined,
                <<"language">> =>[nl],
                <<"name_first">> => undefined,
                <<"name_middle">> => undefined,
                <<"name_surname">> => undefined,
                <<"name_surname_prefix">> => undefined,
                <<"phone">> => undefined,
                <<"phone_mobile">> => undefined,
                <<"email">> => undefined,
                <<"address_city">> => undefined,
                <<"address_country">> => undefined,
                <<"address_postcode">> => undefined,
                <<"address_street_1">> => undefined,
                <<"facebook">> => undefined,
                <<"function">> => undefined,
                <<"linkedin">> => undefined,
                <<"twitter">> => undefined,
                <<"view_location">> => undefined,
                <<"slug">> => undefined,
                <<"website">> => undefined,
                <<"is_published">> => false,
                <<"content_group_id">> => cg_deleted_persons
            },
            m_rsc_update:update(UserId, Props, SudoContext),

            % Delete edges and related objects
            lists:map(
                fun(ObjectId) -> m_rsc:delete(ObjectId, SudoContext) end,
                m_edge:objects(UserId, depiction, SudoContext)
            ),
            m_edge:replace(UserId, hasbanner, [], SudoContext),
            m_edge:replace(UserId, subject, [], SudoContext),

            % Delete identities
            lists:foreach(
                fun(Ident) ->
                    IdentId = proplists:get_value(id, Ident),
                    m_identity:delete(IdentId, SudoContext)
                end,
                m_identity:get_rsc(UserId, SudoContext)
            ),
            % Logoff the person from the site
            Context1 = z_auth:logoff(Context0),
            Context2 = z_authentication_tokens:reset_cookies(Context1),

            Context3 = z_render:wire([{dialog_close, []}], Context2),
            z_render:wire({redirect, [{location, Redirect}]}, Context3);
        _ ->
            Context0
    end.

observe_acl_is_allowed(#acl_is_allowed{} = Allowed, Context) ->
    kenniscloud_acl:is_allowed(Allowed, Context).

observe_acl_is_allowed_prop(#acl_is_allowed_prop{action=_Action, object=Object, prop=Property}, Context) ->
    kenniscloud_acl:is_allowed_prop(Object, Property, Context).

observe_rsc_insert(#rsc_insert{}, Props, Context) ->
    CatId = maps:get(<<"category_id">>, Props, undefined),
    case m_category:id_to_name(CatId, Context) of
        event ->
            Props1 = maps:remove(<<"action_rsvp">>, Props),
            maps:put(<<"action_rsvp">>, <<"1">>, Props1);
        _ ->
            Props
    end.

%% @doc Add remarks on a contribution to the contribution's entity text.
-spec observe_rsc_entity_text(#rsc_entity_text{}, [binary()], z:context()) -> [binary()].
observe_rsc_entity_text(#rsc_entity_text{id = Id}, Acc, Context) ->
    case m_rsc:is_a(Id, contribution, Context) of
        true ->
            Acc ++ m_kc_contribution:entity_texts(Id, Context);
        false ->
            Acc
    end.

observe_rsc_update(#rsc_update{action = _Action, id = Id}, {Modified, Props0}, Context) ->
    Props = maps:remove(<<"is_subjects_checked">>, Props0),
    ExactCategory = m_category:id_to_name(m_rsc:p_no_acl(Id, category_id, Context), Context),
    on_rsc_update({ExactCategory, Id}, {Modified, Props}, Context).

replacing_field(_Field, _Title, undefined) ->
    #{};
replacing_field(Field, undefined, Value) ->
    #{Field => Value};
replacing_field(Field, <<"">>, Value) ->
    #{Field => Value};
replacing_field(_Field, _Title, _Url) ->
    #{}.


valid_embed_url(<<"https://", Url/binary>>) -> valid_embed_url(Url);
valid_embed_url(<<"http://", Url/binary>>) -> valid_embed_url(Url);
valid_embed_url(<<"//", Url/binary>>) -> valid_embed_url(Url);
valid_embed_url(<<"www.youtube.com/", _/binary>>) -> true;
valid_embed_url(<<"youtube.com/", _/binary>>) -> true;
valid_embed_url(<<"www.vimeo.com/", _/binary>>) -> true;
valid_embed_url(<<"vimeo.com/", _/binary>>) -> true;
valid_embed_url(_) -> false.

on_rsc_update({reference, Id}, {_, Props} = Acc, Context) ->
    case maps:get(<<"website">>, Props, m_rsc:p(Id, <<"website">>, Context)) of
        undefined ->
            Acc;
        Url ->
            Referant = m_edge:subject(Id, hasreference, 1, Context),
            case m_rsc:is_a(Referant, contribution, Context) orelse
                 m_rsc:is_a(Referant, reference, Context) of
                true ->
                    update_modified_date(Referant, Context);
                _ ->
                    nil
            end,
            OpenGraphProps = kenniscloud_reference:get_opengraph_data(Url, Context),
            TitleProp = replacing_field(
                <<"title">>,
                z_trans:trans(maps:get(<<"title">>, Props, undefined), Context),
                maps:get(<<"og_title">>, OpenGraphProps, Url)
            ),
            case {valid_embed_url(Url), m_rsc:o(Id, depiction, 1, Context)} of
                {true, undefined} ->
                    EmbedRscProps = #{
                        <<"category">> => video,
                        <<"oembed_url">> => Url,
                        <<"is_published">> => true
                    },
                    {ok, EmbedRsc} = m_rsc:insert(maps:merge(EmbedRscProps, TitleProp), Context),
                    m_edge:insert(Id, depiction, EmbedRsc, Context);
                _ ->
                    ok
            end,
            SummaryProp = replacing_field(
                <<"summary">>,
                z_trans:trans(maps:get(<<"summary">>, Props, undefined), Context),
                maps:get(<<"og_description">>, OpenGraphProps, Url)
            ),

            Props1 = maps:merge(Props, OpenGraphProps),
            Props2 = maps:merge(Props1, TitleProp),
            Props3 = maps:merge(Props2, SummaryProp),

            {ok, Props3}
    end;
on_rsc_update({acl_collaboration_group, Id}, {_, Props}, Context) ->
    PropsWithName =
        case maps:get(<<"name">>, Props, m_rsc:p(Id, name, Context)) of
            undefined ->
                % Only generate a name if none set now or earlier
                Title = z_string:to_slug(maps:get(<<"title">>, Props, undefined)),
                IdBin = z_convert:to_binary(Id),
                NewName = <<"cg_", IdBin/binary, "_", Title/binary>>,
                maps:put(<<"name">>, NewName, Props);
            _ ->
                Props
        end,
    Props2 = maps:put(<<"content_group_id">>, Id, PropsWithName),
    IsPrivateProp = maps:get(<<"is_private">>, Props2, undefined),
    HiddenRuleId = m_kc_collab_group:private_acl_rule_id(Id, true, Context),

    SudoContext = z_acl:sudo(Context),
    case {HiddenRuleId, IsPrivateProp} of
        {RuleId, false} when is_integer(RuleId) ->
            m_acl_rule:delete(rsc, RuleId, SudoContext),
            m_acl_rule:publish(rsc, SudoContext);
        {undefined, true} ->
            m_acl_rule:insert(rsc, m_kc_collab_group:private_acl_rule(Id, Context), SudoContext),
            m_acl_rule:publish(rsc, SudoContext);
        _ -> ok
    end,
    {ok, Props2};
on_rsc_update({region, Id}, {_, Props}, Context) ->
    PropsWithName =
        case maps:get(<<"name">>, Props, m_rsc:p(Id, name, Context)) of
            undefined ->
                % Only generate a name if none set now or earlier
                Title = z_string:to_slug(maps:get(<<"title">>, Props, m_rsc:p(Id, title, Context))),
                IdBin = z_convert:to_binary(Id),
                NewName = <<"region_", IdBin/binary, "_", Title/binary>>,
                maps:put(<<"name">>, NewName, Props);
            _ ->
                Props
        end,
    {ok, PropsWithName};
on_rsc_update({library_keyword, Id}, Acc, Context) ->
    % When a library keyword is inserted/updated, schedule a fetch to find more:
    kenniscloud_azb:update_keywords(Id, Context),
    Acc;
on_rsc_update(_, Acc, _) ->
    Acc.

%% This will update the `modified` date to the current time.
update_modified_date(Id, Context) ->
    m_rsc:touch(Id, Context).

observe_custom_pivot({custom_pivot, Id}, Context0) ->
    SudoContext = z_acl:sudo(Context0),
    HasDepiction = case m_rsc:p(Id, depiction, SudoContext) of
        undefined ->
            false;
        _ ->
            true
    end,
    Props = [{has_depiction, HasDepiction}],
    {kenniscloud_users, Props}.

%% @doc Register activity (when the resource has been published) to send out notifications.
-spec observe_rsc_update_done(#rsc_update_done{}, z:context()) -> ok.
observe_rsc_update_done(#rsc_update_done{id = Id, pre_props = Pre, post_props = Post}, Context) ->
    case {maps:get(<<"is_published">>, Pre, undefined), maps:get(<<"is_published">>, Post, undefined)} of
        {true, true} ->
            ok;
        {_, true} ->
            ok = kenniscloud_activity:maybe_register_activity(Id, Context);
        _ ->
            ok
    end.

observe_driebit_activity2_inserted(#driebit_activity2_inserted{activity = Activity}, Context) ->
    kenniscloud_activity:fan_out(Activity, Context).

% When no request page was specified use '/start'
observe_logon_ready_page(#logon_ready_page{ request_page = None }, Context) when None =:= undefined; None =:= <<>> ->
    z_dispatcher:url_for(start, [], Context);
% Otherwise let the default handler redirect the user
observe_logon_ready_page(_LogonReadyPage, _Context) ->
    undefined.

observe_signup_done(#signup_done{id=NewUserId}, Context) ->
    case z_context:get_q_all(<<"signup_tags">>, Context) of
        [_|_] = TagIds ->
            lists:foreach(
                fun (Tid) ->
                    try z_convert:to_integer(Tid) of
                        TagId -> m_edge:insert(NewUserId, subject, TagId, Context)
                    catch error:badarg ->
                        z:error("Illegal signup_tags id in parameters: ~p", [Tid], [], Context),
                        undefined
                    end
                end,
                TagIds
            );
        _ -> ok
    end.

observe_signup_form_fields(signup_form_fields, FS, _Context) ->
    FS1 = [
        {signup_region, true},
        {signup_tags, false},
        % In Zotonic-0.x Ginger sites, these would have been set by 'mod_ginger_auth'.
        {name_first, false},
        {name_surname_prefix, false},
        {name_surname, false},
        {email, true},
        {block_email, false}
    ],
    % This merges the above proplist on top of the 'FS' accumulator.
    % We do it with maps as there's no straightforward way to merge proplists
    % in Erlang's standard library.
    proplists:from_map(maps:merge(proplists:to_map(FS), proplists:to_map(FS1))).


observe_signup_confirm(#signup_confirm{ id = UserId }, Context) ->
    case m_edge:objects(UserId, hasregion, Context) of
        [] ->
            SignupRegion = m_rsc:p_no_acl(UserId, signup_region, Context),
            case m_rsc:name_to_id(SignupRegion, Context) of
                {ok, RegionId} ->
                    %% todo move to acl's
                    %% is allowed to be changed by user after login in the interface
                    {ok, _} = m_edge:insert(UserId, hasregion, RegionId, z_acl:sudo(Context)),
                    RegionId;
                _ ->
                    undefined
            end;
        _ ->
            ok
    end.

observe_signup_confirm_redirect(#signup_confirm_redirect{}, Context) ->
    z_dispatcher:url_for(start, [], Context).

observe_search_query_term(#search_query_term{ term = <<"cat_exclude_defaults">>, arg = true }, _Context) ->
    #search_sql_term{ cats_exclude = [ {<<"rsc">>, [meta, media, menu, admin_content_query]} ] };
observe_search_query_term(#search_query_term{ term = <<"cat_exclude_defaults">>, arg = false }, _Context) ->
    #search_sql_term{ cats_exclude = [] };
observe_search_query_term(#search_query_term{ term = <<"cat_exclude_defaults">>, arg = Truthy }, Context) ->
    observe_search_query_term(#search_query_term{ term = <<"cat_exclude_defaults">>, arg = z_convert:to_bool(Truthy) }, Context);

% Like 'match_objects' but for multiple input IDs
observe_search_query_term(#search_query_term{ term = <<"match_many_objects">>, arg = IDs }, Context) when is_list(IDs) ->
    MatchObjectIds = #{
        <<"term">> => <<"match_object_ids">>,
        <<"value">> =>
            lists:uniq(lists:flatmap(
                fun(Id) -> m_edge:objects(Id, Context) end,
                IDs
            ))
    },
    IdExcludes = lists:map(
        fun(Id) -> #{<<"term">> => <<"id_exclude">>, <<"value">> => Id} end,
        IDs
    ),
    #{
        <<"operator">> => <<"allof">>,
        <<"terms">> => [MatchObjectIds | IdExcludes]
    };

observe_search_query_term(_, _Context) ->
    undefined.

observe_edge_insert(#edge_insert{predicate=about, subject_id=Subject, object_id=Object}, Context) ->
    ExactCategory = m_category:id_to_name(m_rsc:p_no_acl(Subject, category_id, Context), Context),
    on_edge_insert({ExactCategory, Subject}, about, Object, Context);
observe_edge_insert(#edge_insert{predicate=flag, object_id=Object}, Context) ->
    send_flag_notification(Object, "email_flag_to_region.tpl", Context),
    undefined;
observe_edge_insert(#edge_insert{predicate=hascollabmember, subject_id=GroupId, object_id=UserId}, Context) ->
    case m_rsc:is_a(UserId, person, Context) of
        true ->
            z_email:send_render(
                m_rsc:p(UserId, email, Context),
                "email_welcome_to_group.tpl",
                [
                    {user_id, UserId},
                    {group_id, GroupId},
                    {message, m_rsc:p_no_acl(GroupId, email_welcome_message, Context)}
                ],
                z_acl:sudo(Context)
            ),
            % Cascade the update upwards: when joining a kennisgroep a user will
            % also automatically join the parent kennisgroep(en)
            lists:foreach(
                fun (ParentId) ->
                    % Note: we don't have to worry about preventing loops here, as
                    % 'm_edge:insert' will not raise a notification for existing edges.
                    m_edge:insert(ParentId, hascollabmember, UserId, Context)
                end,
                m_edge:subjects(GroupId, has_subgroup, Context)
            );

        _ ->
            undefined
    end;
observe_edge_insert(#edge_insert{predicate=hascollabmanager, subject_id=GroupId, object_id=UserId}, Context) ->
    case m_rsc:is_a(UserId, person, Context) of
        true ->
            % Cascade the update downwards: becoming a manager of a kennisgroep
            % also automatically makes the user a manager of its subgroups
            lists:foreach(
                fun (SubgroupId) ->
                    % Note: we don't have to worry about preventing loops here, as
                    % 'm_edge:insert' will not raise a notification for existing edges.
                    m_edge:insert(SubgroupId, hascollabmanager, UserId, Context)
                end,
                m_edge:objects(GroupId, has_subgroup, Context)
            );

        _ ->
            undefined
    end;
observe_edge_insert(_, _) ->
    undefined.

on_edge_insert({remark, Subject}, about, Object, Context) ->
    Topic = m_remarks:contribution(Object, Context),
    update_modified_date(Topic, Context),
    Group = m_rsc:p_no_acl(Topic, content_group_id, Context),
    m_rsc:update(Subject, #{<<"content_group_id">> => Group}, z_acl:sudo(Context)),
    kenniscloud_collabgroup_tools:join_group(m_rsc:p_no_acl(Subject, creator_id, Context), Group, Context),
    undefined;
on_edge_insert(_, _, _, _) ->
    undefined.

observe_edge_delete(#edge_delete{predicate=flag, object_id=Object}, Context) ->
    send_flag_notification(Object, "email_unflag_to_region.tpl", Context),
    undefined;
observe_edge_delete(_, _) ->
    undefined.

send_flag_notification(Object, Template, Context) ->
    CollabGroup = m_rsc:p(Object, content_group_id, Context),
    CollabGroupCollection =
        case m_rsc:s(CollabGroup, has_subgroup, Context) of
            [ParentGroup|_] ->
                ParentGroup;
            _ ->
                case m_rsc:o(CollabGroup, hasregion, Context) of
                    [Region|_] -> Region;
                    _ -> undefined
                end
        end,
    case m_rsc:p(CollabGroupCollection, email, Context) of
        undefined ->
            undefined;
        Email ->
            Flaggers = m_rsc:s(Object, flag, Context),
            Vars = [
                {contribution, Object},
                {flags, length(Flaggers)}
            ],
            z_pivot_rsc:insert_task(z_email, send_render, z_ids:id(), [Email, Template, Vars], z_acl:sudo(Context))
    end.

observe_rsc_to_rdf(#rsc_to_rdf{id = RscId}, Triples, Context) ->
    {ok, CatId} = m_category:name_to_id(remark, Context),
    AboutSubjects = m_kc_contribution:transitive_subjects(RscId, about, Context),
    Triples1 = lists:foldl(
        fun(AboutSubject, Acc) ->
            case m_rsc:is_a(AboutSubject, CatId, Context) of
                true ->
                    Triple = #triple{
                                predicate = rdf_property:schema(<<"comment">>),
                                object = m_rsc:p(AboutSubject, uri, Context)
                               },
                    [ Triple | Acc ];
                false ->
                    Acc
            end
        end,
        Triples,
        AboutSubjects),
    AboutObjects = m_edge:objects(RscId, about, Context),
    Triples2 = case m_rsc:is_a(RscId, CatId, Context) of
        true ->
            lists:foldl(
                fun(AboutObject, Acc) ->
                      Triple = #triple{
                                  predicate = rdf_property:schema(<<"parentItem">>),
                                  object = m_rsc:p(AboutObject, uri, Context)
                                 },
                      [ Triple | Acc ]
                end,
                Triples1,
                AboutObjects
             );
        false ->
            Triples1
    end,
    case is_likeable(RscId, Context) of
        true ->
            Upvotes = #triple{
                predicate = rdf_property:schema(<<"upvoteCount">>),
                object = #rdf_value{value = length(m_edge:subjects(RscId, like, Context))}
            },
            [ Upvotes | Triples2 ];
        false ->
            Triples2
    end.

-spec observe_tick_1h(atom(), z:context()) -> any().
observe_tick_1h(tick_1h, Context) ->
    {Today, {H, _M, _S}} = erlang:universaltime(),
    DayOfWeek = calendar:day_of_the_week(Today),
    case {DayOfWeek, H} of
        {2, 10} -> % Tuesdays at 10:00 UTC (11:00/12:00 +01:00)
            kenniscloud_weekly:send(Context);
        {1, 22} -> % Mondays at 22:00 UTC (23:00/24:00 +01:00)
            kenniscloud_azb:update_keywords(Context);
        {_, 1} ->
            generate_hdt_dataset(dataset_contributions, Context);
        {_, 2} -> % Process the most active category at quiet time (3/4 am in our timezone)
            generate_hdt_dataset(dataset_remarks, Context);
        {_, 3} ->
            generate_hdt_dataset(dataset_events, Context);
        {_, 4} ->
            generate_hdt_dataset(dataset_references, Context);
        {_, 5} -> % At 5 utc things are already getting busy in our timezone, so process the least active categories then
            generate_hdt_dataset(dataset_regions, Context),
            generate_hdt_dataset(dataset_knowledge_groups, Context);
        _ ->
            ok
    end.

observe_tick_24h(tick_24h, Context) ->
    % Update the token for AZB:
    kenniscloud_azb:refresh_token(Context),

    % Check all references and if the last update was done more than 'StaleDays'
    % days ago, trigger an update on the same URI ('website').
    StaleDays = z_convert:to_integer(m_config:get_value(site, stale_reference_days, 7, Context)),
    Now = calendar:universal_time(),
    % Note: if nothing has changed (including the metadata fetched in 'on_rsc_update'),
    % then this leaves the resource unmodified.
    m_category:foreach(
        reference,
        fun(RscId, Ctx) ->
            Website = m_rsc:p(RscId, <<"website">>, Ctx),
            LastModified = m_rsc:p(RscId, <<"modified">>, Ctx),
            SinceLastUpdate = z_datetime:diff(LastModified, Now),
            case SinceLastUpdate >= {{0, 0, StaleDays}, {0, 0, 0}} of
                true ->
                    m_rsc:update(RscId, #{<<"website">> => Website}, Ctx);
                false ->
                    ok
            end
        end,
        % Note: we use sudo here to have the permission to update all references
        % (regardless of who initially created them).
        z_acl:sudo(Context)
    ).

observe_validate_subjects({validate_subjects, {postback, Id, Value, Args}}, Context) ->
    case proplists:get_value(<<"id">>, Args, undefined) of
        undefined ->
            {{ok, Value}, Context};
        Rsc ->
            case m_rsc:o(Rsc, subject, Context) of
                L when length(L) > 17 ->
                    TruncedSubjects = lists:sublist(L, 17),
                    m_edge:replace(Rsc, subject, TruncedSubjects, z_acl:sudo(Context)),
                    Context1 = z_render:growl_error("Voeg maximaal 17 tags toe", Context),
                    {{error, Id, "Voeg maximaal 17 tags toe"}, Context1};
                [_,_,_,_|_] ->
                    {{ok, Value}, Context};
                _ ->
                    Context1 = z_render:growl_error("Voeg minstens 4 tags toe", Context),
                    {{error, Id, "Voeg minstens 4 tags toe"}, Context1}
            end
    end.

-spec is_likeable(m_rsc:resource(), z:context()) -> boolean().
is_likeable(Rsc, Context) ->
    {ok, Like} = m_predicate:name_to_id(like, Context),
    lists:any(
        fun(C) ->
            m_rsc:is_a(Rsc, C, Context)
        end,
        m_predicate:objects(Like, Context)
    ).
