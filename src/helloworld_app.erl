%%%-------------------------------------------------------------------
%% @doc helloworld public API
%% @end
%%%-------------------------------------------------------------------

-module(helloworld_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	helloworld_gui:start_link(),
    helloworld_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
