-module(rabbitmq_samples).

-export([start/0]).

start() ->
    ok = application:start(rabbitmq_samples).