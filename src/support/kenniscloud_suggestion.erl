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


%% @doc Create link between contribution and a library publication.
-spec event(#postback{}, z:context()) -> z:context().
event(#postback{message = {confirm_suggestion, Args}, target = TargetId}, Context) ->
    Triple = #triple{
        subject = maps:get(<<"subject">>, Args, undefined),
        predicate = <<"http://purl.org/dc/terms/references">>,
        object = maps:get(<<"uri">>, Args, undefined),
        object_props = []
    },
    {ok, _} = m_rdf_triple:insert(Triple, Context),

    Target = z_convert:to_binary(TargetId),
    z_render:wire(
        {script, [
            {script, [<<"$('#", Target/binary, "').parent().slideUp()">>]}
        ]},
        Context
    ).
