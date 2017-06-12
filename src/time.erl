%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 13.02
%%%-------------------------------------------------------------------
-module(time).
-author("sarunas").

%% API
-export([get_next_run_date/1, add_time/2, get_execute_time/1]).

get_next_run_date(Expression) ->
  Parts = split(Expression),
  MappedParts = map(Parts),
  FilteredParts = filter(MappedParts),
  IndexedParts = index(FilteredParts, [], 1),

%% Round off time till 00 seconds
  CurrentTime = calendar:local_time(),
  {_Date, {_H, _I, S}} = CurrentTime,
  CurrentTimeApprox = add_time(CurrentTime, (60 - S)),

  search(1, length(IndexedParts), IndexedParts, CurrentTimeApprox)
.

%% Check is cron expression valid and explode it
split(Expression) ->
  Parts = string:tokens(Expression, " "),
  case length(Parts) of
    5 -> Parts;
    _ -> incorrect_cron_expression
  end.

map([Minutes, Hours, DayOfMonth, Months, DayOfWeek]) ->
  [{months, Months}, {day_of_month, DayOfMonth}, {day_of_week, DayOfWeek}, {hours, Hours}, {minutes, Minutes}].

%% Remove values with * (any value)
filter(MappedParts) ->
  Fun = fun(X) ->
    {_Key, Value} = X,
    Value =/= "*"
  end,
  lists:filter(Fun, MappedParts).

%% Index and map
%% todo map?
index([], Result, _Iteration) ->
  lists:reverse(Result);
index([{Module, Part}|Rest], Result, Iteration) ->
  FormattedPart = parser:process({Module, Part}),
  index(Rest, [{Iteration, {Module, FormattedPart}}|Result], Iteration + 1).

%% Main search recursion
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

add_time(DateTime, Seconds) ->
  calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime) + Seconds).

get_execute_time(Expression) ->
  NextRunGregorianSeconds = calendar:datetime_to_gregorian_seconds(get_next_run_date(Expression)),
  CurrentGregorianSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  ExecuteAfter = NextRunGregorianSeconds - CurrentGregorianSeconds,
  ExecuteAfter.