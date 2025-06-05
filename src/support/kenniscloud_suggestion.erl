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

%% @doc Suggestions from external sources that are shown with contributions.
-module(kenniscloud_suggestion).

-export([
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Create link between contribution and a library publication.
-spec event(#postback{}, z:context()) -> z:context().
event(#postback{message = {confirm_suggestion, [{subject, SubjectId}, {uri, Uri}]}, target = TargetId}, Context) ->
    kenniscloud_reference:insert_opengraph_data(Uri, SubjectId, Context),

    Target = z_convert:to_binary(TargetId),
    z_render:wire(
        {script, [
            {script, [<<"$('#", Target/binary, "').parent().slideUp()">>]}
        ]},
        Context
    ).
