-module(hello_rabbitmq_handler).

-behaviour(gen_event).

-include_lib("deps/amqp_client/include/amqp_client.hrl").

%% API
-export([add_handler/0, delete_handler/0]).

%% Behaviour
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {channel}).

%% API implementation

%% @spec add_handler() -> ok.
%% @doc Add current module as handler to server.
add_handler() ->
	hello_rabbitmq_event:add_handler(?MODULE, []).
%% @spec delete_handler() -> ok
%% @doc Delete handler from server.
delete_handler() ->
	hello_rabbitmq_event:delete_handler(?MODULE, []).


%% Callbacks
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

init([]) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = <<"hello">>}),
    io:format(" [*] Waiting for messages.~n"),

    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = <<"hello">>}, self()),
	{ok, #state{channel=Channel}}.


%% Rabbitmq events handling.
handle_info(#'basic.consume_ok'{}, State) ->
	{ok, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Body}}, State) ->
    Dots = length([C || C <- binary_to_list(Body), C == $.]),
    io:format(" [x] Received ~p~n", [Body]),
    receive
    after
        Dots*1000 -> ok
    end,
    io:format(" [x] Done~n"),
    amqp_channel:cast(State#state.channel, #'basic.ack'{delivery_tag = Tag}),
    {ok, State}.

