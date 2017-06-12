%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2016 21.23
%%%-------------------------------------------------------------------
-module(job).
-author("sarunas").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_TIME_SEND_AFTER, 4233600). %% 49 days

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link(?MODULE, [], []).

start_link(Arguments) ->
  gen_server:start_link(?MODULE, Arguments, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

%% todo ar galima TimeToExecute sukišti į state?
%% todo peržiūrėti sintaksę: ar f-jos gale taškas naujoje eilutėje; ar tarp f-jų su skirtingomis deklaracijomis reikia tarpo;
%% todo kaip eunit failus sugeneruoti kitoje direktorijoje?
init([{TimeToExecute, Fun}]) ->
  check_job(TimeToExecute),
  {ok, Fun};

init(_) ->
  {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({execute}, {func, Fun}) ->
  Fun(),
  exit(normal),
  {noreply, []};

handle_info({execute}, {mfa, M, F, A}) ->
  M:F(A),
  exit(normal),
  {noreply, []};

handle_info({continue, TimeToExecute}, Job) ->
  check_job(TimeToExecute),
  {noreply, Job}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_job(TimeToExecute) ->
  case TimeToExecute > ?MAX_TIME_SEND_AFTER of
    true ->
      CallAfterS = TimeToExecute - ?MAX_TIME_SEND_AFTER,
      erlang:send_after(?MAX_TIME_SEND_AFTER * 1000, self(), {continue, CallAfterS});
    _ ->
      erlang:send_after(TimeToExecute * 1000, self(), {execute})
  end.