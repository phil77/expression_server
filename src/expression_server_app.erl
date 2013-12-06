-module(expression_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    Apps = [syntax_tools, compiler, goldrush, lager, ranch, expression_server],
    [ ok = application:start(App) || App <- Apps],
    ok.

start(_StartType, _StartArgs) ->
    expression_server_sup:start_link().

stop(_State) ->
    ok.
