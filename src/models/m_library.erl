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

-module(m_library).

-export([
    m_get/3
]).

-behaviour(zotonic_model).
-include("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) ->
    {ok, { term(), list() }} | {error, term()}.

% Syntax: m.library.suggestion[keywords]
m_get([ <<"suggestions">>, Keywords | Rest ], _Msg, Context) when is_list(Keywords) ->
    {ok, {kenniscloud_azb:fetch_suggestions(Keywords, Context), Rest}};

% Unexpected query
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

