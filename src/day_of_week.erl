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
-export([
    process/2
]).



%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Find next day of week from cron expression
%%
process({Date, Time}, SearchingDayOfWeek) ->
    DayOfWeek = calendar:day_of_the_week(Date),
    IsInList = lists:member(DayOfWeek, SearchingDayOfWeek),
    case IsInList of
        true  -> {{Date, Time}, false};
        _     -> {increment({Date, {0, 0, 0}}, SearchingDayOfWeek), true}
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Day of week search recursive function
%%
increment({Date, Time}, SearchingDayOfWeek) ->
    {NewDate, NewTime} = time:add_time({Date, Time}, 86400),
    DayOfWeek = calendar:day_of_the_week(NewDate),
    IsInList = lists:member(DayOfWeek, SearchingDayOfWeek),
    case IsInList of
        true  -> {NewDate, NewTime};
        _     -> increment({NewDate, NewTime}, SearchingDayOfWeek)
    end.



%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    [
        ?_assertEqual(process({{2016,9,7},{12,14,1}},    [7]),        {{{2016,9,11},{0,0,0}},    true}),
        ?_assertEqual(process({{2016,10,18},{10,20,15}}, [1]),        {{{2016,10,24},{0,0,0}},   true}),
        ?_assertEqual(process({{2017,2,18},{10,20,15}},  [6]),        {{{2017,2,18},{10,20,15}}, false}),
        ?_assertEqual(process({{2018,6,14},{11,20,15}},  [1, 6, 7]),  {{{2018,6,16},{0,0,0}},    true})
    ].

-endif.


