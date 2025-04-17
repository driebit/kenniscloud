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

-module(kenniscloud_utils).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    ensure_trans/1,
    prop_text/3
]).


%% @doc ensure that the given value is wrapped in a 'trans' record.
ensure_trans(undefined) -> ensure_trans(<<"">>);
ensure_trans(Tr) when is_record(Tr, trans) -> Tr;
ensure_trans(Tr) -> #trans{ tr = [{en, z_convert:to_binary(Tr)}, {nl, z_convert:to_binary(Tr)}]}.



%% Combines the `m_rsc:p` of the given textual property with:
%% 1. extracting the text itself if it's in a translation
%% 2. that the text has sanitized html
%% Defaults to an empty binary string.
-spec prop_text(Resource, Property, Context) -> binary() when
    Resource :: z:resource(),
    Property :: atom() | binary() | string(),
    Context :: z:context().
prop_text(RscId, Property, Context) ->
    case z_trans:lookup(m_rsc:p(RscId, Property, Context), Context) of
        Trans when is_binary(Trans) -> z_sanitize:html(Trans);
        _ -> <<"">>
    end.
