%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 03.09
%%%-------------------------------------------------------------------
-module(erl_cron_minutes).
-author("sarunas").

%% API
-export([
    process/2
]).



%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Find next minutes from cron expression
%%
process({Date, {H, I, S}}, SearchingMinute) ->
  IsInList = lists:member(I, SearchingMinute),
  case IsInList of
    true  -> {{Date, {H,I,S}}, false};
    _     -> {increment({Date, {H, I, 0}}, SearchingMinute), true}
  end.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Minutes search recursive function
%%
increment({Date, {H, I, S}}, SearchingMinute) ->
    {NewDate, {NewHours, NewMinutes, NewSeconds}} = erl_cron_time:add_time({Date, {H, I, S}}, 60),
    IsInList = lists:member(NewMinutes, SearchingMinute),
    case IsInList of
        true  -> {NewDate,{NewHours,NewMinutes,0}};
        _     -> increment({NewDate, {NewHours, NewMinutes, NewSeconds}}, SearchingMinute)
    end.



%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    [
        ?_assertEqual(process({{2016,9,7},{12,14,1}},    [8]),      {{{2016,9,7},{13,8,0}},    true}),
        ?_assertEqual(process({{2016,10,18},{23,57,15}}, [50]),     {{{2016,10,19},{0,50,0}},  true}),
        ?_assertEqual(process({{2017,2,18},{10,20,15}},  [20]),     {{{2017,2,18},{10,20,15}}, false}),
        ?_assertEqual(process({{2018,6,14},{10,20,15}},  [12, 18]), {{{2018,6,14},{11,12,0}},  true})
    ].

-endif.


