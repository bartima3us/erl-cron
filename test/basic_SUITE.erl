%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2017 08.49
%%%-------------------------------------------------------------------
-module(basic_SUITE).
-include_lib("eunit/include/eunit.hrl").
-author("sarunas").

%% API
-export([all/0]).
-export([test1/1, test2/1]).

all() -> [test1,test2].

test1(_Config) ->
  1 = 1.

test2(_Config) ->
  A = 0,
  1/A.

