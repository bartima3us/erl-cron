%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 03.09
%%%-------------------------------------------------------------------
-module(months).
-author("sarunas").

%% API
-export([
    process/2
]).



%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Find next months from cron expression
%%
process({{Y, M, D}, Time}, SearchingMonth) ->
    IsInList = lists:member(M, SearchingMonth),
    case IsInList of
        true -> {{{Y, M, D}, Time}, false};
        _ -> {increment({{Y, M, D}, {0,0,0}}, SearchingMonth), true}
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Months search recursive function
%%
increment({{Y, M, _D}, Time}, SearchingMonth) ->
    LastDayOfMonth = calendar:last_day_of_the_month(Y, M),
    {{NewYear, NewMonth, NewDay}, Time} = time:add_time({{Y, M, LastDayOfMonth}, Time}, 86400),
    IsInList = lists:member(NewMonth, SearchingMonth),
    case IsInList of
        true  -> {{NewYear, NewMonth, NewDay},{0,0,0}};
        _     -> increment({{NewYear, NewMonth, NewDay}, Time}, SearchingMonth)
    end.



%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    [
        ?_assertEqual(process({{2016,5,7},{12,14,1}}, [8]),       {{{2016,8,1},{0,0,0}},   true}),
        ?_assertEqual(process({{2016,5,7},{12,14,1}}, [4, 8, 9]), {{{2016,8,1},{0,0,0}},   true}),
        ?_assertEqual(process({{2016,5,7},{12,14,1}}, [5]),       {{{2016,5,7},{12,14,1}}, false}),
        ?_assertEqual(process({{2016,5,7},{12,14,1}}, [4, 5, 6]), {{{2016,5,7},{12,14,1}}, false})
    ].

-endif.


