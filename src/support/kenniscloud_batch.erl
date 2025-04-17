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

% todo; can we remove this? does not seem to be used?
-module(kenniscloud_batch).

-export([
    change_category_of_any_anonymous_person_to_anonymous_participant/1
]).

-include("zotonic_core/include/zotonic.hrl").

change_category_of_any_anonymous_person_to_anonymous_participant(Context) ->
    SudoContext = z_acl:sudo(Context),
    import_utils_bulk:for_categorized_rsc(
        fun(PersonId, _) ->
               m_rsc:update(PersonId, [{category, anonymous_participant}], SudoContext)
        end,
        anonymous_person,
        SudoContext
    ).
