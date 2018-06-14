%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 03.09
%%%-------------------------------------------------------------------
-module(erl_cron_hours).
-author("sarunas").

%% API
-export([
    process/2
]).



%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Find next hours from cron expression
%%
process({Date, {H, I, S}}, SearchingHour) ->
    IsInList = lists:member(H, SearchingHour),
    case IsInList of
        true  -> {{Date, {H,I,S}}, false};
        _     -> {increment({Date, {H, 0, 0}}, SearchingHour), true}
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Hours search recursive function
%%
increment({Date, {H, I, S}}, SearchingHour) ->
    {NewDate, {NewHours, NewMinutes, NewSeconds}} = erl_cron_time:add_time({Date, {H, I, S}}, 3600),
    IsInList = lists:member(NewHours, SearchingHour),
    case IsInList of
        true  -> {NewDate,{NewHours,0,0}};
        _     -> increment({NewDate, {NewHours, NewMinutes, NewSeconds}}, SearchingHour)
    end.



%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    [
        ?_assertEqual(process({{2016,5,7},{12,14,1}},   [8]),      {{{2016,5,8},{8,0,0}},   true}),
        ?_assertEqual(process({{2016,5,8},{12,14,1}},   [14]),     {{{2016,5,8},{14,0,0}},  true}),
        ?_assertEqual(process({{2016,5,8},{14,14,1}},   [14]),     {{{2016,5,8},{14,14,1}}, false}),
        ?_assertEqual(process({{2018,6,14},{10,20,15}}, [12, 18]), {{{2018,6,14},{12,0,0}}, true})
    ].

-endif.


