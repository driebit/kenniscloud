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

-module(controller_start).
-author("Driebit <tech@driebit.nl>").

-export([
    is_authorized/1,
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).

is_authorized(Context) ->
    Id = z_controller_helper:get_configured_id(Context),
    z_controller_helper:is_authorized(Id, Context).

resource_exists(Context) ->
    {false, Context}.

previously_existed(Context) ->
    {true, Context}.

%% @doc Location to redirects a user to after logon/signup.
%% Dispatched on "/start", used in:
%% - the "_auth_link.tpl" template
%% - 'kenniscloud:observe_logon_ready_page/2'
%% - 'kenniscloud:observe_signup_confirm_redirect/2'
moved_temporarily(Context) ->
    Location = case z_acl:user(Context) of
        undefined -> z_dispatcher:abs_url(<<"/">>, Context);
        UserId ->
            % If a valid region was specified at signup, redirect to the region page
            % Else redirect to the profile page
            NoRegion = m_rsc:rid(region_none, Context),
            case m_edge:objects(UserId, hasregion, Context) of
                [ NoRegion | _ ] -> m_rsc:page_url_abs(UserId, Context);
                [ RegionId | _ ] -> m_rsc:page_url_abs(RegionId, Context);
                _Other -> m_rsc:page_url_abs(UserId, Context)
            end
    end,
    {{true, Location}, Context}.
