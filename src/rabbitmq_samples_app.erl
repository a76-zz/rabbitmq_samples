-module(rabbitmq_samples_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("Application is started.~n"),
    rabbitmq_samples_sup:start_link().

stop(_State) ->
    ok.
