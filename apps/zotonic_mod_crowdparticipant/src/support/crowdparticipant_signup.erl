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

-module(crowdparticipant_signup).
-author("Driebit <tech@driebit.nl>").

-export([
    mk_participant_data/9,
    participant_data_to_map/1,
    is_valid/2,

    to_signup_url/3,
    from_signup_url/2,

    encode_participant/2,
    decode_participant/2
]).

-include("zotonic_core/include/zotonic.hrl").

-record(
    participant_data,
        { name :: string() | binary()
        , crowd_id :: m_rsc:resource_id()
        , email_address :: string() | binary()
        , show_contact :: boolean()
        , keywords :: list(m_rsc:resource_id())
        , custom_keywords :: list(string()) | list(binary())
        , linkedin_name :: string() | binary()
        , instagram_name :: string() | binary()
        , expiration :: calendar:datetime()
        }
    ).

%% @doc Utility function to build 'participant_data' records.
mk_participant_data(Name, CrowdId, EmailAddress, ShowContact, Keywords, CustomKeywords, LinkedinName, InstagramName, Expiration) ->
    #participant_data
        { name=Name
        , crowd_id=CrowdId
        , email_address=EmailAddress
        , show_contact=ShowContact
        , keywords=Keywords
        , custom_keywords=CustomKeywords
        , linkedin_name=LinkedinName
        , instagram_name=InstagramName
        , expiration=Expiration
        }.

%% @doc Utility function to convert a 'participant_data' record to a map with binary keys.
participant_data_to_map(#participant_data{
                           name = Name,
                           crowd_id = CrowdId,
                           email_address = EmailAddress,
                           show_contact = ShowContact,
                           keywords = Keywords,
                           custom_keywords = CustomKeywords,
                           linkedin_name = LinkedinName,
                           instagram_name = InstagramName,
                           expiration = Expiration
                          }) ->
    #{ <<"name">> => Name,
       <<"crowd">> => CrowdId,
       <<"email">> => EmailAddress,
       <<"show_contact">> => ShowContact,
       <<"keywords">> => Keywords,
       <<"custom_keywords">> => CustomKeywords,
       <<"linkedin_name">> => LinkedinName,
       <<"instagram_name">> => InstagramName,
       <<"expiration">> => Expiration
     }.

%% @doc Check that all the validators on the fields of the 'participant_data'
%% have their conditions satisfied.
-spec is_valid(participant_data, z:context()) -> boolean() | {false, term()}.
is_valid(ParticipantData, Context) ->
    Name = ParticipantData#participant_data.name,
    VName = validator_crowdparticipant_name:validate(crowdparticipant_name, no_id, Name, [], Context),

    CrowdId = ParticipantData#participant_data.crowd_id,
    VCrowdId = validator_crowdparticipant_crowd:validate(crowdparticipant_crowd, no_id, CrowdId, [], Context),

    EmailAddress = ParticipantData#participant_data.email_address,
    VEmailAddress = validator_crowdparticipant_email:validate(crowdparticipant_email, no_id, EmailAddress, [], Context),

    VKeywords = lists:map(
        fun(K) -> validator_crowdparticipant_keyword:validate(crowdparticipant_keyword, no_id, K, [], Context) end,
        ParticipantData#participant_data.keywords),

    VCustomKeywords = lists:map(
        fun(K) -> validator_crowdparticipant_custom_keyword:validate(crowdparticipant_custom_keyword, no_id, K, [], Context) end,
        ParticipantData#participant_data.custom_keywords),

    % search for an error in any of the validations
    VComplete = [VName, VCrowdId, VEmailAddress] ++ VKeywords ++ VCustomKeywords,
    case lists:search(fun({{error, _, _}, _}) -> true; (_) -> false end, VComplete) of
        false -> true;
        {value, {{error, _, Value}, _}} -> {false, Value}
    end.


%% @doc Generate a unique URL for signup.
%% This attaches an encoded query to the provided base URL.
%% NOTE: this doesn't validate the 'participant_data', use 'is_valid/2'.
-spec to_signup_url(string(), participant_data, z:context()) -> string().
to_signup_url(BaseURL, ParticipantData, Context) ->
    EncodedData = encode_participant(ParticipantData, Context),
    EncodedQuery = mochiweb_util:urlencode([{"participant", EncodedData}]),
    {Scheme, Netloc, Path, _, Fragment} = mochiweb_util:urlsplit(z_convert:to_list(BaseURL)),
    mochiweb_util:urlunsplit({Scheme, Netloc, Path, EncodedQuery, Fragment}).

%% @doc Obtain the 'participant_data' from a signup URL.
%% This looks for the encoded data in the URL query.
%% NOTE: this doesn't validate the 'participant_data', use 'is_valid/2'.
-spec from_signup_url(string(), z:context()) -> {ok, participant_data} | {error, term()}.
from_signup_url(URL, Context) ->
    {_, _, _, Query, _} = mochiweb_util:urlsplit(z_convert:to_list(URL)),
    QueryProps = mochiweb_util:parse_qs(Query),
    case proplists:get_value("participant", QueryProps) of
        undefined -> {error, nodata};
        EncodedData -> decode_participant(EncodedData, Context)
    end.


%% @doc Encode the 'participant_data' to a Base64 protected value.
%% The original 'participant_data' can be re-obtained with 'decode_participant'.
%% NOTE: this doesn't validate the 'participant_data', use 'is_valid/2'.
-spec encode_participant(participant_data, z:context()) -> binary().
encode_participant(ParticipantData, Context) ->
    ExpirationDate = ParticipantData#participant_data.expiration,
    z_utils:encode_value_expire(ParticipantData, ExpirationDate, Context).

%% @doc Decode the 'participant_data' from a Base64 protected value.
%% Returns an error if the 'expiration' date has passed.
%% NOTE: this doesn't validate the 'participant_data', use 'is_valid/2'.
-spec decode_participant(term(), z:context()) -> participant_data | {error, term()}.
decode_participant(Data, Context) ->
    case z_utils:decode_value_expire(Data, Context) of
        {ok, ParticipantData} -> ParticipantData;
        Error -> Error
    end.
