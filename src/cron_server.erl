%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------
-module(cron_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_jobs/0,
    add_job/1,
    delete_job/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

%% Internal state
-record(cron, {
    jobs = dict:new()
}).



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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



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
init([]) ->
    {ok, #cron{}}.


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
handle_call({add, Expression, Job}, _From, JobsList) ->
    RunDate = time:get_next_run_date(Expression),
    TimeToExecute = time:get_execute_time(Expression),
    {ok, Pid} = job:start_link([{TimeToExecute, Job}]),
    MonitorRef = erlang:monitor(process, Pid),
    NewJobsList = dict:store(MonitorRef, {Pid, RunDate, Job}, JobsList#cron.jobs),
    {reply, ok, JobsList#cron{jobs = NewJobsList}};

handle_call(get, _From, JobsList) ->
    Jobs = lists:reverse(dict:to_list(JobsList#cron.jobs)),
    {reply, {ok, Jobs}, JobsList};

handle_call({delete, MonitorRef}, _From, JobsList) ->
    NewJobsList = dict:erase(MonitorRef, JobsList#cron.jobs),
    {reply, ok, NewJobsList}.


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
handle_info({'DOWN', MonitorRef, process, _Pid, normal}, JobsList) ->
    NewJobsList = dict:erase(MonitorRef, JobsList#cron.jobs),
    {noreply, JobsList#cron{jobs = NewJobsList}}.


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

%% @doc
%% Add new job to cron server
%%
add_job({Expression, Fun}) ->
    ok = gen_server:call(?MODULE, {add, Expression, {func, Fun}});

add_job({Expression, M, F, A}) ->
    ok = gen_server:call(?MODULE, {add, Expression, {mfa, M, F, A}});

add_job(_) ->
    {error, bad_job_definition}.


%% @doc
%% Get all jobs list
%%
get_jobs() ->
    {ok, Jobs} = gen_server:call(?MODULE, get),
    Jobs.


%% @doc
%% Delete job from list
%%
delete_job(MonitorRef) ->
    ok = gen_server:call(?MODULE, {delete, MonitorRef}).


