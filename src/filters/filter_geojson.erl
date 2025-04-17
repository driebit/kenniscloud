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

-module(filter_geojson).
-export([geojson/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

geojson(#search_result{result = Resources}, Context) ->
    Features = lists:filtermap(fun(Resource) -> makeFeature(Resource, Context) end, Resources),
    #{
        <<"type">> => <<"FeatureCollection">>,
        <<"features">> => Features
    };
geojson(Resources, Context) ->
    Features = lists:filtermap(fun(Resource) -> makeFeature(Resource, Context) end, Resources),
    #{
        <<"type">> => <<"FeatureCollection">>,
        <<"features">> => Features
    }.

makeFeature(Resource, Context) ->
    CategoryId = m_rsc:p(Resource, "category_id", Context),
    CategoryName = string:lowercase(m_rsc:p(CategoryId, "name", Context)),
    makePointFeature(Resource, CategoryName, Context).

makePointFeature(Resource, CategoryName, Context) ->
    Lng = m_rsc:p(Resource, "pivot_location_lng", Context),
    Lat = m_rsc:p(Resource, "pivot_location_lat", Context),
    case {Lat, Lng} of
        {undefined, _} ->
            false;
        {_, undefined} ->
            false;
        _ ->
            {true, #{
                <<"type">> => <<"Feature">>,
                <<"geometry">> => #{
                    <<"type">> => <<"Point">>,
                    <<"coordinates">> => [Lng, Lat]
                },
                <<"properties">> => #{
                    <<"category">> => CategoryName,
                    <<"id">> => Resource
                }
            }}
    end.
