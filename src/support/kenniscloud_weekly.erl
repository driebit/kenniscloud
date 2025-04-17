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

-module(kenniscloud_weekly).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    send/1,
    send/2,
    unsubscribe/1,
    transpose/1
]).

-spec send(z:context()) -> ok.
send(Context) ->
    case m_config:get_boolean(kenniscloud, send_weekly, false, Context) of
        false ->
            z:info("Weekly update not sent because it is disabled in config kenniscloud send_weekly", Context),
            ok;
        true ->
            z:info("Weekly update is being queued for sending", Context),
            send_in_batches(Context)
    end.

send_in_batches(Context) ->
    % Fold through all the resources that are a 'person'
    {_, LastBatch} = m_category:fold(
        person,
        fun (RscId, Acc, _Context) ->
            case Acc of
                % Every 200 resources, insert a new task
                {200, CurrentBatch} ->
                    insert_batch_task(CurrentBatch, Context),
                    {1, [RscId]};
                % Otherwise collect the resources in a batch
                {N, CurrentBatch} ->
                    {N+1, [ RscId | CurrentBatch]}
            end
        end,
        {0, []},
        Context
    ),
    % Finally, insert a task for the last batch (possibly less than 200 ids)
    insert_batch_task(LastBatch, Context).

insert_batch_task([], _Context) -> ok;
insert_batch_task(UserBatch, Context) when is_list(UserBatch) ->
    Key = z_ids:id(),
    z_pivot_rsc:insert_task(kenniscloud_weekly, send, Key, [UserBatch], Context),
    ok.

-spec send(integer() | list(integer()), z:context()) -> noop | ok.
send(Users, Context) when is_list(Users) ->
    lists:foreach(fun(User) -> send_if_desired(User, Context) end, Users);
send(User, Context) when is_integer(User) ->
    case m_rsc:p(User, email_raw, Context) of
        undefined ->
            ok;
        Email ->
            News = find_content(news, User, Context),
            Updates = find_content(contributions, User, Context),
            MeetupUpcoming = find_content(meetup_upcoming, User, Context),
            MeetupNext = find_content(meetup_next, User, Context),
            case {News, Updates,  MeetupUpcoming, MeetupNext} of
                {[], [], undefined, undefined} ->
                    noop;
                _ ->
                    UnsubscribeUrl = z_dispatcher:url_for(
                        unsubscribe,
                        [{mailing, weekly}, {absolute_url, true}],
                        Context
                    ),
                    Vars = [
                        {user_id, User},
                        {news, News},
                        {updates, Updates},
                        {meetup_upcoming, MeetupUpcoming},
                        {meetup_next, MeetupNext},
                        {unsubscribe_url, UnsubscribeUrl}
                    ],
                    z_email:send_render(Email, "email_weekly.tpl", Vars, z_acl:sudo(Context)),
                    ok
            end
    end.

days_from_now(Days) ->
    {Today, _Time} = erlang:universaltime(),
    {calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Today) + Days), {0,0,0}}.

% Given a matrix (as a list of lists), converts columns for rows (and vice-versa).
transpose([]) -> [];
transpose([[]|Xss]) -> transpose(Xss);
transpose([[X|Xs]|Xss]) -> [[X|[H||[H|_] <- Xss]]|transpose([Xs|[T||[_|T] <- Xss]])].

% Given a list of sub-lists, skims the top 'N' elements from the sublists.
% IOW this prioritizes the first element of each sub-lists, then the second ones,
% then the third ones, etc. until 'N' elements are found in total.
skim_top_elements(N, GroupedResults) ->
    SortedElements = lists:flatten(transpose(GroupedResults)),
    {Result, _Rest} = z_utils:split(N, SortedElements),
    Result.

find_content(news, User, Context) ->
    % include news published in the past week, up to and including today
    DefaultContentGroupId = m_rsc:rid(default_content_group, Context),
    News = find_content(cg_news, DefaultContentGroupId, Context),
    NewsPerGroup = lists:map(
        fun(GroupId) -> find_content(cg_news, GroupId, Context) end,
        m_kc_user:knowledge_groups(User, Context)
    ),
    % Feature 3 items skimming the most recent from each group
    GroupsNews = skim_top_elements(3, NewsPerGroup),
    News++GroupsNews;
find_content(cg_news, ContentGroupId, Context) ->
    #search_result{result = News} = m_search:search(
        {query, [
            {cat, news},
            {content_group, ContentGroupId},
            {sort, "-rsc.publication_start"},
            {filter, [publication_start, lte, days_from_now(0)]},
            {filter, [publication_start, gte, days_from_now(-6)]},
            {pagelen, 3}
        ]},
        Context
    ),
    News;

find_content(meetup_upcoming, User, Context) ->
    find_content(meetup, {User, days_from_now(25), days_from_now(31)}, Context);
find_content(meetup_next, User, Context) ->
    find_content(meetup, {User, days_from_now(1), days_from_now(7)}, Context);
find_content(meetup, {User, From, To}, Context) ->
    Regions = [ [R, hasregion] || R <- m_kc_user:regions(User, Context)],
    SearchResult = m_search:search(
        {query, [
            {cat, event},
            {sort, "rsc.pivot_date_start"},
            {hasanyobject, Regions},
            {filter, [creator_id, ne, User]},
            {filter, [pivot_date_start, gte, From]},
            {filter, [pivot_date_start, lte, To]},
            {pagelen, 1}
        ]},
        Context
    ),
    case SearchResult of
        #search_result{result = [Result|_]} -> Result;
        _ -> undefined
    end;

find_content(contributions, User, Context) ->
    Groups = m_kc_user:knowledge_groups(User, Context),
    ResultsPerGroup = lists:map(
        fun(Group) ->
            #search_result{result = Result} = m_search:search({query, [
                {content_group, Group},
                {sort, "-created"},
                {cat_exclude, remark},
                {filter, [creator_id, ne, User]},
                {filter, [created, gte, days_from_now(-7)]},
                {pagelen, 3}
            ]}, Context),
            Result
        end,
        Groups
    ),
    % Prefer more active groups (see AC of issue KC-206):
    SortedResultsPerGroup = lists:sort(
        fun(A, B) ->
            length(A) > length(B)
        end,
        ResultsPerGroup
    ),
    % Feature 3 items skimming the most recent from each group
    skim_top_elements(3, SortedResultsPerGroup).


-spec send_if_desired(integer(), z:context()) -> noop | ok.
send_if_desired(User, Context) when is_integer(User) ->
    % Enable weekly newsletter by default (if not configured for user)
    case m_rsc:p(User, receive_weekly_update_mail, true, Context) of
        true -> send(User, Context);
        false -> ok
    end.

-spec unsubscribe(z:context()) -> {ok, z:resource()} | {error, term()}.
unsubscribe(Context) ->
    User = z_acl:user(Context),
    m_rsc:update(User, [{receive_weekly_update_mail, false}], Context).
