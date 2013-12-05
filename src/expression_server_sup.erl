-module(expression_server_sup).

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
    RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
        permanent, 5000, supervisor, [ranch_sup]},
    ListenerSpec = ranch:child_spec(echo, 100,
        ranch_tcp, [{port, 1234}],
        myprotocol, []
    ),
    {ok, { {one_for_one, 5, 10}, [RanchSupSpec, ListenerSpec] } }.

