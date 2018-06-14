%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 03.10
%%%-------------------------------------------------------------------
-module(erl_cron_day_of_month).
-author("sarunas").

%% API
-export([
    process/2
]).



%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Find next day of month from cron expression
%%
process({{Y, M, D}, Time}, SearchingDayOfMonth) ->
    IsInList = lists:member(D, SearchingDayOfMonth),
    case IsInList of
        true  -> {{{Y, M, D}, Time}, false};
        _     -> {increment({{Y, M, D}, {0, 0, 0}}, SearchingDayOfMonth), true}
    end.


%% @doc
%% Day of month search recursive function
%%
increment({{Y, M, D}, Time}, SearchingDayOfMonth) ->
    {{NewYear, NewMonth, NewDay}, NewTime} = erl_cron_time:add_time({{Y, M, D}, Time}, 86400),
    IsInList = lists:member(NewDay, SearchingDayOfMonth),
    case IsInList of
        true  -> {{NewYear, NewMonth, NewDay}, NewTime};
        _     -> increment({{NewYear, NewMonth, NewDay}, NewTime}, SearchingDayOfMonth)
    end.



%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    [
        ?_assertEqual(process({{2016,9,7},{12,14,1}},    [8]),          {{{2016,9,8},{0,0,0}},    true}),
        ?_assertEqual(process({{2016,10,18},{10,20,15}}, [1]),          {{{2016,11,1},{0,0,0}},    true}),
        ?_assertEqual(process({{2017,2,18},{10,20,15}},  [18]),         {{{2017,2,18},{10,20,15}}, false}),
        ?_assertEqual(process({{2018,6,14},{10,20,15}},  [12, 18, 19]), {{{2018,6,18},{0,0,0}},    true})
    ].

-endif.


