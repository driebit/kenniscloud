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

%% return total remarks count

-module(filter_kc_count_remarks).
-export([kc_count_remarks/3]).
-include("zotonic_core/include/zotonic.hrl").

kc_count_remarks(Ids, Predicate, Context) when is_list(Ids) ->
    lists:foldl(fun(Id, Sum) ->
            length(get_published_remarks(Id, Predicate, Context)) + Sum
        end,
        length(Ids),
        Ids);

kc_count_remarks(Id, Predicate, Context) ->
    Ids = get_published_remarks(Id, Predicate, Context),
    kc_count_remarks(Ids, Predicate, Context).

get_published_remarks(Id, Predicate, Context) ->
    SubjectIds = m_edge:subjects(Id, Predicate, Context),
    lists:filter(
        fun(SubjectId) ->
            m_rsc:p(SubjectId, is_published, Context)
        end,
        SubjectIds
    ).
