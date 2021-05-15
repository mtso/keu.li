%%%-------------------------------------------------------------------
%% @doc keuli public API
%% @end
%%%-------------------------------------------------------------------

-module(keuli_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = keuli_sup:start_link(),
    Routes = [ {
        '_',
        [
            {"/", keuli_handler, []}
        ]
    } ],
    Dispatch = cowboy_router:compile(Routes),
    NumAcceptors = 10,
    Port = os:getenv("PORT", "3000"),
    TransOpts = [ {ip, {0,0,0,0}}, {port, list_to_integer(Port)} ],
    ProtoOpts = #{ env => #{ dispatch => Dispatch } },
    {ok, _} = cowboy:start_clear(chicken_poo_poo,
        TransOpts, ProtoOpts),
    {ok, Pid}.

stop(_State) ->
    ok.

%% internal functions
