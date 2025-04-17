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

-module(validator_crowdparticipant_email).
-author("Driebit <tech@driebit.nl>").

-export([
    validate/5,
    render_validator/5
]).

-include("zotonic_core/include/zotonic.hrl").

%% @doc Validates the 'email_address' of a crowd participant.
%% Verifies that the email address fits the allowed format.
%%
%% Event handling is set up to handle this as a postback form validator, so to
%% check an input field with id 'email' it's sufficient to add to the template:
%% {% validate id="email" type={postback event="crowdparticipant_email"} %}
validate(crowdparticipant_email, Id, EmailAddress, Args, Context) ->
    validator_base_email:validate(email, Id, EmailAddress, Args, Context).

render_validator(email, TriggerId, TargetId, Args, Context) ->
    validator_base_email:render_validator(email, TriggerId, TargetId, Args, Context).
