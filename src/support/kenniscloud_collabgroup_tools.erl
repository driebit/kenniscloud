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

-module(kenniscloud_collabgroup_tools).
-include("zotonic_core/include/zotonic.hrl").

-export([
    event/2,
    join_group/3
]).

event(#submit{message={new_collabgroup, _Args}}, Context) ->
    GroupName = z_context:get_q_validated(<<"name">>, Context),
    case sudo_add_group(GroupName, Context) of
        {ok, Id} ->
            Context1 = z_render:growl("Kennisgroep aangemaakt", Context),
            z_render:wire({redirect, [{dispatch, admin_edit_rsc}, {id, Id}]}, Context1);
        {error, Reason} ->
            z:error(Reason, Context),
            z_render:growl_error("Kon de kennisgroep niet aanmaken", Context)
    end.

sudo_add_group(Name, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            Props = #{
                <<"is_published">> => true,
                <<"category">> => acl_collaboration_group,
                <<"title">> => iolist_to_binary(Name),
                <<"creator_id">> => z_acl:user(Context)
            },
            m_rsc:insert(Props, z_acl:sudo(Context));
        false ->
            {error, eacces}
    end.

% defensive programming because downstream cannot handle undefineds
join_group(undefined, _, _) ->
    ok;
join_group(_, undefined, _) ->
    ok;
join_group(User, Group, Context) ->
    case {m_rsc:is_a(User, person, Context), m_rsc:is_a(Group, acl_collaboration_group, Context)} of
        {true, true} ->
            case m_edge:insert(Group, hascollabmember, User, z_acl:sudo(Context)) of
                {ok, _} -> ok;
                _ -> undefined
            end;
        _ ->
            undefined
    end.
