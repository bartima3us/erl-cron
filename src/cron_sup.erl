-module(cron_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Server1 = #{id => server1,                  % id mandatory
        start => {cron_server,start_link,[]},   % {M,F,A}  mandatory
        restart => permanent,                   % optional: permanent,temporary, transient, defaults to permanent
        shutdown => 5000,                       % optional: brutal_kill, integer(), defaults to 5000 if type==worker else infinity
        type => worker,                         % optional: worker, supervisor, defaults to worker
        modules => [cron_server]                % optional: list of modules for hot code upgrade
    },
    {ok, { {one_for_one, 5, 10}, [Server1]} }.


