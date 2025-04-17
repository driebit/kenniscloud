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

%% @doc A contribution to a knowledge group.
-module(m_kc_contribution).

%% API
-export([
    remarks/2,
    entity_texts/2,
    transitive_subjects/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Get all remarks about a contribution, including nested ones.
-spec remarks(m_rsc:resource(), z:context()) -> [m_rsc:resource()].
remarks(Id, Context) ->
    lists:filter(
        fun(About) ->
            m_rsc:is_a(About, remark, Context)
        end,
        %% Do a transitive lookup because remarks can be about remarks, too.
        transitive_subjects(Id, about, Context)
    ).

%% @doc Get texts to be used for entity recognition.
-spec entity_texts(m_rsc:resource(), z:context()) -> [binary()].
entity_texts(Id, Context) ->
    remark_texts(Id, Context) ++ tag_titles(Id, Context).

%% @doc Get remark body texts.
-spec remark_texts(m_rsc:resource(), z:context()) -> [binary()].
remark_texts(Id, Context) ->
    lists:map(
        fun(Remark) -> kenniscloud_utils:prop_text(Remark, body, Context) end,
        remarks(Id, Context)
    ).

%% @doc Get tag titles.
-spec tag_titles(m_rsc:resource(), z:context()) -> [binary()].
tag_titles(Id, Context) ->
    lists:map(
        fun(Keyword) -> kenniscloud_utils:prop_text(Keyword, title, Context) end,
        m_edge:objects(Id, subject, Context)
    ).

%% @doc Recursively get transitive subjects of a resource.
-spec transitive_subjects(m_resource:resource(), atom(), z:context()) -> [m_rsc:resource()].
transitive_subjects(Id, Predicate, Context) ->
    [_Self|Subjects] = transitive_subjects([Id], Predicate, [], Context),
    Subjects.

transitive_subjects([], _Predicate, Acc, _Context) ->
    lists:usort(Acc);
transitive_subjects([Id | Ids], Predicate, Acc, Context) ->
    %% Prevent recursion because of self-reference.
    Subjects = case lists:member(Id, Acc) of
        false ->
            m_edge:subjects(Id, Predicate, Context);
        true ->
            []
    end,
    transitive_subjects(Ids ++ Subjects, Predicate, [Id | Acc], Context).
