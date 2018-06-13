%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 03.09
%%%-------------------------------------------------------------------
-module(hours).
-author("sarunas").

%% API
-export([process/2, hours_test/0]).


%%
%%
%%
process({Date, {H, I, S}}, SearchingHour) ->
    IsInList = lists:member(H, SearchingHour),
    case IsInList of
        true  -> {{Date, {H,I,S}}, false};
        _     -> {increment({Date, {H, 0, 0}}, SearchingHour), true}
    end.


%%
%%
%%
increment({Date, {H, I, S}}, SearchingHour) ->
    {NewDate, {NewHours, NewMinutes, NewSeconds}} = time:add_time({Date, {H, I, S}}, 3600),
    IsInList = lists:member(NewHours, SearchingHour),
    case IsInList of
        true  -> {NewDate,{NewHours,0,0}};
        _     -> increment({NewDate, {NewHours, NewMinutes, NewSeconds}}, SearchingHour)
    end.


%% Test
hours_test() ->
    {{{2016,5,8},{8,0,0}}, true} = process({{2016,5,7},{12,14,1}}, [8]),
    {{{2016,5,8},{14,0,0}}, true} = process({{2016,5,8},{12,14,1}}, [14]),
    {{{2016,5,8},{14,0,0}}, false} = process({{2016,5,8},{14,14,1}}, [14]).


