%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------
-module(erl_cron_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_jobs/0,
    add_job/1,
    delete_by_name/1,
    delete_by_ref/1,
    delete_all/0
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

-record(job, {
    name,           % Job name
    ref,            % Unique job ref (for user operations)
    monitor_ref,    % Internal monitor ref
    pid,            % Job process pid
    task,           % Task (MFA or anonymous function)
    expression,     % Cron expression
    run_date        % Next run date
}).

%% Internal state
-record(data, {
    jobs = [] :: [#job{}]   % Jobs list
}).



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
    {ok, #data{}}.


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
handle_call({add, Expression, Task, Name}, _From, Data = #data{jobs = CurrentJobs}) ->
    RunDate = erl_cron_time:get_next_run_date(Expression),
    NewJob = #job{ref = Ref} = create_job(Name, make_ref(), Task, Expression, RunDate),
    {reply, {ok, Ref}, Data#data{jobs = [NewJob|CurrentJobs]}};

handle_call(get, _From, Data = #data{jobs = Jobs}) ->
    JobsReversed = lists:reverse(Jobs),
    {reply, {ok, JobsReversed}, Data};

handle_call({delete_by_ref, Ref}, _From, Data = #data{jobs = Jobs}) ->
    {value, #job{pid = Pid}} = lists:keysearch(Ref, #job.ref, Jobs),
    exit(Pid, kill),
    {reply, ok, Data};

handle_call({delete_by_name, Name}, _From, Data = #data{jobs = Jobs}) ->
    {value, #job{pid = Pid}} = lists:keysearch(Name, #job.name, Jobs),
    exit(Pid, kill),
    {reply, ok, Data};

handle_call(delete_all, _From, Data = #data{jobs = Jobs}) ->
    lists:map(fun (#job{pid = Pid}) -> exit(Pid, kill) end, Jobs),
    {reply, ok, Data}.


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
handle_info({'DOWN', MonitorRef, process, _Pid, killed}, Data = #data{jobs = Jobs}) ->
    {value, OldJob} = lists:keysearch(MonitorRef, #job.monitor_ref, Jobs),
    NewJobsList = lists:delete(OldJob, Jobs),
    {noreply, Data#data{jobs = NewJobsList}};

handle_info({'DOWN', MonitorRef, process, _Pid, normal}, Data = #data{jobs = Jobs}) ->
    {value, OldJob} = lists:keysearch(MonitorRef, #job.monitor_ref, Jobs),
    NewJobsList = lists:delete(OldJob, Jobs),
    #job{
        name        = Name,
        ref         = Ref,
        task        = Task,
        expression  = Expression
    } = OldJob,
    RunDate = erl_cron_time:get_next_run_date(Expression),
    NewJob = create_job(Name, Ref, Task, Expression, RunDate),
    {noreply, Data#data{jobs = [NewJob|NewJobsList]}}.


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


%% @doc
%% Add new job to cron server
%%
add_job({Expression, Fun}) when is_function(Fun) ->
    {ok, _Ref} = gen_server:call(?MODULE, {add, Expression, {func, Fun}, undefined});

add_job({Expression, {M, F, A}}) ->
    {ok, _Ref} = gen_server:call(?MODULE, {add, Expression, {mfa, M, F, A}, undefined});

add_job({Expression, Fun, Name}) when is_function(Fun) ->
    {ok, _Ref} = gen_server:call(?MODULE, {add, Expression, {func, Fun}, Name});

add_job({Expression, {M, F, A}, Name}) ->
    {ok, _Ref} = gen_server:call(?MODULE, {add, Expression, {mfa, M, F, A}, Name});

add_job(_) ->
    {error, bad_job_definition}.


%% @doc
%% Get all jobs list
%%
get_jobs() ->
    {ok, Jobs} = gen_server:call(?MODULE, get),
    Jobs.


%% @doc
%% Delete job from list by reference
delete_by_ref(Ref) ->
    ok = gen_server:call(?MODULE, {delete_by_ref, Ref}).


%% @doc
%% Delete job from list by name
%%
delete_by_name(Name) ->
    ok = gen_server:call(?MODULE, {delete_by_name, Name}).


%% @doc
%% Delete all jobs
%%
delete_all() ->
    ok = gen_server:call(?MODULE, delete_all).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Create new job record
%%
create_job(Name, Ref, Task, Expression, RunDate) ->
    TimeToExecute = erl_cron_time:get_execute_time(Expression),
    {ok, Pid} = erl_cron_job:start_link([{TimeToExecute, Task}]),
    MonitorRef = erlang:monitor(process, Pid),
    #job{
        name        = Name,
        ref         = Ref,
        monitor_ref = MonitorRef,
        pid         = Pid,
        task        = Task,
        expression  = Expression,
        run_date    = RunDate
    }.


