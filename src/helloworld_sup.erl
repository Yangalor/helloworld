%%%-------------------------------------------------------------------
%% @doc helloworld top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(helloworld_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	
%%	SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
%%    ChildSpecs = [#{id => gui,
%%                    start => {helloworld_gui, start_link, []},
%%                    restart => transient,
%%                    shutdown => brutal_kill,
%%                    type => worker,
%%                    modules => [helloworld_gui]}],
%%    {ok, {SupFlags, ChildSpecs}}.
	{ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
