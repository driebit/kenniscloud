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

-module(filter_kc_domain_name_from_url).
-export([kc_domain_name_from_url/2]).
-include("zotonic_core/include/zotonic.hrl").

kc_domain_name_from_url(Url, _Context) when is_binary(Url) orelse is_list(Url) ->
    case uri_string:parse(Url) of
        #{host := Host} -> Host;
        _ -> undefined
    end;
kc_domain_name_from_url(_, _) ->
    undefined.
