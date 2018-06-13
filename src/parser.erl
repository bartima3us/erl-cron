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
-export([process/1]).

%%
%%
%%
process({Module, Part}) ->
    TokensByComma = string:tokens(Part, ","),
    Explode = fun (Pt) -> explode(Pt, Module) end,
    Tokens = lists:map(Explode, TokensByComma),
    lists:append(Tokens).


%%
%%
%%
explode(Part, Module) ->
    IsRange = string:str(Part, "-"),
    case IsRange of
        0 ->
            IsIncrementRange = string:str(Part, "/"),
            case IsIncrementRange of
                0 ->
                    [list_to_integer(Part), []];
                _ ->
                    [Start, IncStep] = string:tokens(Part, "/"),
                    Offset = proplists:get_value(Module, offsets()),
                    case Start of
                        "*" ->
                            Values = lists:seq(0, Offset, list_to_integer(IncStep)),
                            [[]|Values];
                        _ ->
                            Values = lists:seq(list_to_integer(Start), Offset, list_to_integer(IncStep)),
                            [[]|Values]
                    end
          end;
      _ ->
          [Start, End] = string:tokens(Part, "-"),
          IsIncrementRange = string:str(Part, "/"),
          case IsIncrementRange of
              0 ->
                  Values = lists:seq(list_to_integer(Start), list_to_integer(End)),
                  [[]|Values];
              _ ->
                  [Offset, IncStep] = string:tokens(End, "/"),
                  Values = lists:seq(list_to_integer(Start), list_to_integer(Offset), list_to_integer(IncStep)),
                  [[]|Values]
          end
    end.


%%
%%
%%
offsets() ->
    [{minutes, 60}, {hours, 24}, {day_of_month, 31}, {months, 12}, {day_of_week, 7}].


