-module(proxy_client).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    application:start(ranch),
    proxy_client_sup:start_link().

stop(_State) ->
    lager:stop(),
    ok.

