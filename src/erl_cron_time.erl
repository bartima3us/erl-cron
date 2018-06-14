%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 13.02
%%%-------------------------------------------------------------------
-module(erl_cron_time).
-author("sarunas").

%% API
-export([
    get_next_run_date/1,
    add_time/2,
    get_execute_time/1
]).



%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Get next run date by cron expression
%%
get_next_run_date(Expression) when is_list(Expression) ->
    Parts = split(Expression),
    MappedParts = map(Parts),
    FilteredParts = filter(MappedParts),
    IndexedParts = index(FilteredParts, [], 1),
    % Round off time till 00 seconds
    CurrentTime = calendar:local_time(),
    {_Date, {_H, _I, S}} = CurrentTime,
    CurrentTimeApprox = add_time(CurrentTime, (60 - S)),
    search(1, length(IndexedParts), IndexedParts, CurrentTimeApprox).


%% @doc
%% Convert datetime to Gregorian seconds, add seconds and then convert seconds back to datetime
%%
add_time(DateTime = {{_,_,_},{_,_,_}}, Seconds) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime) + Seconds).


%% @doc
%% Get seconds till next cron run by cron expression
%%
get_execute_time(Expression) when is_list(Expression) ->
    NextRunGregorianSeconds = calendar:datetime_to_gregorian_seconds(get_next_run_date(Expression)),
    CurrentGregorianSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    ExecuteAfter = NextRunGregorianSeconds - CurrentGregorianSeconds,
    ExecuteAfter.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Check is cron expression valid and explode it
%%
split(Expression) when is_list(Expression) ->
    Parts = string:tokens(Expression, " "),
    case length(Parts) of
        5 -> Parts;
        _ -> {error, incorrect_cron_expression}
    end.


%% @doc
%% Make proplist of time units
%%
map([Minutes, Hours, DayOfMonth, Months, DayOfWeek]) ->
    [
        {erl_cron_months, Months},
        {erl_cron_day_of_month, DayOfMonth},
        {erl_cron_day_of_week, DayOfWeek},
        {erl_cron_hours, Hours},
        {erl_cron_minutes, Minutes}
    ].


%% @doc
%% Remove values with * (any value)
%%
filter(MappedParts) ->
    Fun = fun({_Key, Value}) ->
        Value =/= "*"
    end,
    lists:filter(Fun, MappedParts).


%% @doc
%% Index and map
%% @todo map?
index([], Result, _Iteration) ->
    lists:reverse(Result);

index([{Module, Part}|Rest], Result, Iteration) ->
    FormattedPart = erl_cron_parser:process({Module, Part}),
    index(Rest, [{Iteration, {Module, FormattedPart}}|Result], Iteration + 1).


%% @doc
%% Main search recursion
%%
search(_IteratingId, 0, _Parts, ResultDateTime) ->
    ResultDateTime;

search(IteratingId, StopIndex, Parts, ResultDateTime) ->
    Part = proplists:get_value(IteratingId, Parts),
    CountParts = length(Parts),
    {Module, SearchingValue} = Part,
    {NewResultDateTime, IsIncremented} = Module:process(ResultDateTime, SearchingValue),
    %% Count stop index which starts from elements in cron expression count
    NewStopIndex = case IsIncremented of
        true -> CountParts;
        _ -> StopIndex - 1
    end,
    NextIteratingId = case IteratingId of
        CountParts -> 1;
        _ -> IteratingId + 1
    end,
    search(NextIteratingId, NewStopIndex, Parts, NewResultDateTime).



%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    [
        ?_assertEqual(add_time({{2018,7,1},{12,15,10}}, 100), {{2018,7,1},{12,16,50}})
    ].

-endif.


