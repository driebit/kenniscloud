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

-module(mod_crowdlink).

-mod_title("Crowd Link").
-mod_description("Crowd Links for Meetup Crowd").
-mod_author("Driebit").
-mod_schema(1).

-export([
    observe_tick_1h/2,
    observe_admin_menu/3,
    observe_rsc_update/3,
    manage_schema/2,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

observe_tick_1h(tick_1h, Context) ->
    m_crowdlink:clean_old(Context),
    m_crowdlink:set_daycrowds(Context).

observe_admin_menu(#admin_menu{}, Acc, _Context) ->
    [#menu_item{
        id = admin_crowdlink_overview,
        parent = admin_modules,
        label = "Crowd Links",
        url = {crowdlink_admin_overview},
        visiblecheck = {acl, use, mod_admin}
       }
    | Acc ].

observe_rsc_update(#rsc_update{id = Id}, Acc, Context) ->
    m_crowdlink:set_expiry(Id, Context),
    Acc.

event({postback, {delete_crowdlink, [{id, Id}]}, _TriggerId, _TargetId}, Context) ->
    m_crowdlink:delete(Id, Context),
    z_render:wire({reload, []}, Context);

event({postback, {set_crowdlink, [{id, Id}]}, _TriggerId, _TargetId}, Context) ->
    _ = m_crowdlink:set(Id, Context),
    z_render:wire({reload, []}, Context);

event({postback, {reset_daycrowd, [{id, Id}]}, _TriggerId, _TargetId}, Context) ->
    m_crowdlink:set_daycrowd(Id, Context),
    z_render:wire({reload, []}, Context).

manage_schema(_Version, Context) ->
    ok = m_crowdlink:install(Context).
