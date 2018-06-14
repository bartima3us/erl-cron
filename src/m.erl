%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2017 11.13
%%%-------------------------------------------------------------------
-module(m).
-author("sarunas").

%% API
-export([
  f/1
]).


%% @doc
%% Function for test purposes
%%
f(A) ->
  io:format("done ~p~n", [A]).


