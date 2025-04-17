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

%% @doc filter to return a css background-position based on crop_center values
-module(filter_background_position).

-export([background_position/2]).

-include("zotonic_core/include/zotonic.hrl").

-spec background_position(integer(), z:context()) -> binary().
background_position(Id, Context) ->
    case m_rsc:p_no_acl(Id, <<"medium_edit_settings">>, Context) of
        #{<<"crop_center_x">> := +0.0, <<"crop_center_y">> := +0.0} ->
            % Note: we check this condition as both values will be 0.0
            % when the center position is reset from the CMS.
            "center center";
        #{<<"crop_center_x">> := CropX, <<"crop_center_y">> := CropY} ->
            PosX =
                if
                    CropX < 0.34 -> "left";
                    CropX > 0.66 -> "right";
                    true -> "center"
                end,
            PosY =
                if
                    CropY < 0.34 -> "top";
                    CropY > 0.66 -> "bottom";
                    true -> "center"
                end,
            PosX++" "++PosY;
        _Other ->
            "center center"
    end.
