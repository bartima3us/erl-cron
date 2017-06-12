%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 03.09
%%%-------------------------------------------------------------------
-module(minutes).
-author("sarunas").

%% API
-export([process/2, test/0]).

process({Date, {H, I, S}}, SearchingMinute) ->

  IsInList = lists:member(I, SearchingMinute),

  case IsInList of
    true -> {{Date, {H,I,S}}, false};
    _ -> {increment({Date, {H, I, 0}}, SearchingMinute), true}
  end.

increment({Date, {H, I, S}}, SearchingMinute) ->
  {NewDate, {NewHours, NewMinutes, NewSeconds}} = time:add_time({Date, {H, I, S}}, 60),
  IsInList = lists:member(NewMinutes, SearchingMinute),

  case IsInList of
    true -> {NewDate,{NewHours,NewMinutes,0}};
    _ -> increment({NewDate, {NewHours, NewMinutes, NewSeconds}}, SearchingMinute)
  end.

%% Test
test() ->
  {{{2016,9,7},{13,8,0}}, true} = process({{2016,9,7},{12,14,1}}, 8),
  {{{2016,10,19},{0,50,0}}, true} = process({{2016,10,18},{23,57,15}}, 50),
  {{{2017,2,18},{10,20,15}}, false} = process({{2017,2,18},{10,20,15}}, 20).