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

-module(kenniscloud_dbpedia).

-export([
    get_resource/2
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

%% @doc Retrieve DBpedia resource properties that we need for Kenniscloud.
-spec get_resource(binary(), z:context()) -> #rdf_resource{} | undefined.
get_resource(Uri, Context) ->
    Properties = [
        rdf_property:rdfs(<<"label">>),
        rdf_property:'dbpedia-owl'(<<"thumbnail">>),
        rdf_property:'dbpedia-owl'(<<"abstract">>),
        rdf_property:foaf(<<"isPrimaryTopicOf">>),
        <<"http://nl.dbpedia.org/property/naam">>
    ],
    ?DEBUG(Uri),
    case dbpedia:get_resource(Uri, Properties, <<"nl">>) of
        undefined ->
            undefined;
        #rdf_resource{} = RdfResource ->
            supplement_triples(RdfResource, Context)
    end.

%% @doc Supplement DBpedia triples from Wikipedia if needed.
-spec supplement_triples(m_rdf:rdf_resource(), z:context()) -> m_rdf:rdf_resource().
supplement_triples(RdfResource, Context) ->
    case needs_supplemental_wikipedia_data(RdfResource) of
        false ->
            RdfResource;
        true ->
            case m_rdf:objects(RdfResource, rdf_property:foaf(<<"isPrimaryTopicOf">>)) of
                [] ->
                    RdfResource;
                [WikipediaUrl | _] ->
                    Props = kenniscloud_reference:get_opengraph_data(WikipediaUrl, Context),
                    RdfResource#rdf_resource{
                        triples = RdfResource#rdf_resource.triples ++ [
                            #triple{
                                predicate = rdf_property:'dbpedia-owl'(<<"thumbnail">>),
                                object = proplists:get_value(og_image, Props)
                            },
                            #triple{
                                predicate = rdf_property:rdfs(<<"label">>),
                                object = proplists:get_value(og_title, Props)
                            }
                        ]
                    }
            end
    end.

-spec needs_supplemental_wikipedia_data(m_rdf:rdf_resource()) -> boolean().
needs_supplemental_wikipedia_data(#rdf_resource{ triples = Triples }) ->
    m_rdf:lookup_triple(thumbnail, Triples) =:= undefined
    orelse m_rdf:lookup_triple(title, Triples) =:= undefined.
