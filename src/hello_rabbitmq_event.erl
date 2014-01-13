-module(hello_rabbitmq_event).

-export([start_link/0, add_handler/2, delete_handler/2]).

-define(SERVER, ?MODULE).

%% API implementation

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts event server.

start_link() ->
	gen_event:start_link({local, ?SERVER}).

%% @spec add_handler(Handler::atom()) -> ok | {'EXIT', Reason} | term()
%% @doc Add handler to server.
add_handler(Handler, Args) ->
	gen_event:add_handler(?SERVER, Handler, Args).

%% @spec delete_handler(Handler::atom()) -> ok | {'EXIT', Reason} | term()
%% @doc Delete handler from server.
delete_handler(Handler, Args) ->
	gen_event:delete_handler(?SERVER, Handler, Args).