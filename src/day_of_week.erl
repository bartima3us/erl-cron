%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 03.10
%%%-------------------------------------------------------------------
-module(day_of_week).
-author("sarunas").

%% API
-export([process/2, test/0]).

%%calendar:day_of_the_week

process({Date, Time}, SearchingDayOfWeek) ->
  DayOfWeek = calendar:day_of_the_week(Date),
  IsInList = lists:member(DayOfWeek, SearchingDayOfWeek),

  case IsInList of
    true -> {{Date, Time}, false};
    _ -> {increment({Date, {0, 0, 0}}, SearchingDayOfWeek), true}
  end.

increment({Date, Time}, SearchingDayOfWeek) ->
  {NewDate, NewTime} = time:add_time({Date, Time}, 86400),
  DayOfWeek = calendar:day_of_the_week(NewDate),
  IsInList = lists:member(DayOfWeek, SearchingDayOfWeek),

  case IsInList of
    true -> {NewDate, NewTime};
    _ -> increment({NewDate, NewTime}, SearchingDayOfWeek)
  end.

%% Test
test() ->
  {{{2016,9,11},{0,0,0}}, true} = process({{2016,9,7},{12,14,1}}, 7),
  {{{2016,10,24},{0,0,0}}, true} = process({{2016,10,18},{10,20,15}}, 1),
  {{{2017,2,18},{10,20,15}}, false} = process({{2017,2,18},{10,20,15}}, 6).