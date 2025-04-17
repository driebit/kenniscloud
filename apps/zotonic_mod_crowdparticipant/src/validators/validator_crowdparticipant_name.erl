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

-module(validator_crowdparticipant_name).
-author("Driebit <tech@driebit.nl>").

-export([validate/5]).

-include("zotonic_core/include/zotonic.hrl").

%% @doc Validates the 'name' of a crowd participant.
%% Rejects names that aren't between 1 and 100 chars in length or that contain
%% non digalpha chars.
%%
%% Event handling is set up to handle this as a postback form validator, so to
%% check an input field with id 'name' it's sufficient to add to the template:
%% {% validate id="name" type={postback event="crowdparticipant_name"} %}
validate(crowdparticipant_name, Id, Name, _Args, Context) ->
    % We look for differences (case-ignoring) with the result of
    % `z_string:to_name/1`, which removes all disallowed characters.
    % Problem with that though is that spaces also are not allowed and
    % we probably should allow them.
    % Comparing the length of the resulting name instead of directly
    % comparing with the original might be best to mitigate that.
    ValidName = z_string:to_name(Name),
    case string:length(Name) == string:length(ValidName) of
        false -> {{error, Id, invalid_chars}, Context};
        true ->
            Min = 1,
            Max = 100,
            validator_base_length:validate(length, Id, Name, [Min,Max], Context)
    end.
