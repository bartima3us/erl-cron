%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2016 03.50
%%%-------------------------------------------------------------------
-module(parser).
-author("sarunas").

%% API
-export([
    process/1
]).



%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Main parser function
%%
process({Module, Part}) ->
    TokensByComma = string:tokens(Part, ","),
    Explode = fun (Pt) -> explode(Pt, Module) end,
    Tokens = lists:map(Explode, TokensByComma),
    lists:append(Tokens).



%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Split expression parts
%%
explode(Part, Module) ->
    IsRange = string:str(Part, "-"),
    case IsRange of
        0 ->
            IsIncrementRange = string:str(Part, "/"),
            case IsIncrementRange of
                0 ->
                    [list_to_integer(Part)];
                _ ->
                    [Start, IncStep] = string:tokens(Part, "/"),
                    Offset = proplists:get_value(Module, offsets()),
                    case Start of
                        "*" ->
                            lists:seq(0, Offset, list_to_integer(IncStep));
                        _ ->
                            lists:seq(list_to_integer(Start), Offset, list_to_integer(IncStep))
                    end
          end;
        _ ->
            [Start, End] = string:tokens(Part, "-"),
            IsIncrementRange = string:str(Part, "/"),
            case IsIncrementRange of
                0 ->
                    lists:seq(list_to_integer(Start), list_to_integer(End));
                _ ->
                    [Offset, IncStep] = string:tokens(End, "/"),
                    lists:seq(list_to_integer(Start), list_to_integer(Offset), list_to_integer(IncStep))
            end
    end.


%% @doc
%% Time unit offsets
%%
offsets() ->
    [{minutes, 60}, {hours, 24}, {day_of_month, 31}, {months, 12}, {day_of_week, 7}].



%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    [
        ?_assertEqual(process({minutes,      "6,9-20/2"}),    [6,9,11,13,15,17,19]),
        ?_assertEqual(process({minutes,      "*/5"}),         [0,5,10,15,20,25,30,35,40,45,50,55,60]),
        ?_assertEqual(process({minutes,      "10-15"}),       [10,11,12,13,14,15]),
        ?_assertEqual(process({minutes,      "14"}),          [14]),
        ?_assertEqual(process({minutes,      "8,9"}),         [8,9]),
        ?_assertEqual(process({hours,        "7,8,10-16"}),   [7,8,10,11,12,13,14,15,16]),
        ?_assertEqual(process({hours,        "7,8"}),         [7,8]),
        ?_assertEqual(process({hours,        "*/3"}),         [0,3,6,9,12,15,18,21,24]),
        ?_assertEqual(process({hours,        "8-10"}),        [8,9,10]),
        ?_assertEqual(process({hours,        "15"}),          [15]),
        ?_assertEqual(process({day_of_month, "7,10-14"}),     [7,10,11,12,13,14]),
        ?_assertEqual(process({day_of_month, "7,8"}),         [7,8]),
        ?_assertEqual(process({day_of_month, "*/6"}),         [0,6,12,18,24,30]),
        ?_assertEqual(process({day_of_month, "1-5"}),         [1,2,3,4,5]),
        ?_assertEqual(process({day_of_month, "27"}),          [27]),
        ?_assertEqual(process({months,       "5,9-12/2"}),    [5,9,11]),
        ?_assertEqual(process({months,       "7,9,12"}),      [7,9,12]),
        ?_assertEqual(process({months,       "*/6"}),         [0,6,12]),
        ?_assertEqual(process({months,       "1-4"}),         [1,2,3,4]),
        ?_assertEqual(process({months,       "6"}),           [6]),
        ?_assertEqual(process({day_of_week,  "7,10-14/2"}),   [7,10,12,14]),
        ?_assertEqual(process({day_of_week,  "1,2,3"}),       [1,2,3]),
        ?_assertEqual(process({day_of_week,  "*/3"}),         [0,3,6]),
        ?_assertEqual(process({day_of_week,  "2-6"}),         [2,3,4,5,6]),
        ?_assertEqual(process({day_of_week,  "6"}),           [6])
    ].

-endif.


