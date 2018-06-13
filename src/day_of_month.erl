%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 03.10
%%%-------------------------------------------------------------------
-module(day_of_month).
-author("sarunas").

%% API
-export([process/2, test/0]).


%%
%%
%%
process({{Y, M, D}, Time}, SearchingDayOfMonth) ->
    IsInList = lists:member(D, SearchingDayOfMonth),
    case IsInList of
        true  -> {{{Y, M, D}, Time}, false};
        _     -> {increment({{Y, M, D}, {0, 0, 0}}, SearchingDayOfMonth), true}
    end.


%%
%%
%%
increment({{Y, M, D}, Time}, SearchingDayOfMonth) ->
    {{NewYear, NewMonth, NewDay}, NewTime} = time:add_time({{Y, M, D}, Time}, 86400),
    IsInList = lists:member(NewDay, SearchingDayOfMonth),
    case IsInList of
        true  -> {{NewYear, NewMonth, NewDay}, NewTime};
        _     -> increment({{NewYear, NewMonth, NewDay}, NewTime}, SearchingDayOfMonth)
    end.


%% Test
test() ->
    {{{2016,9,8},{0,0,0}}, true} = process({{2016,9,7},{12,14,1}}, 8),
    {{{2016,11,1},{0,0,0}}, true} = process({{2016,10,18},{10,20,15}}, 1),
    {{{2017,2,18},{10,20,15}}, false} = process({{2017,2,18},{10,20,15}}, 18).


