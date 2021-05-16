%%%-------------------------------------------------------------------
%% @doc keuli public API
%% @end
%%%-------------------------------------------------------------------

-module(keuli_app).

-behaviour(application).

-export([start/2, stop/1, read_priv_file/1]).

read_priv_file(Filename) ->
    case code:priv_dir(keuli) of
        {error, bad_name} ->
            io:format("bad file name!"),
            PrivDir = "apps/keuli/priv";
        PrivDir -> ok
    end,
    file:read_file(filename:join([PrivDir, Filename])).

get_pg_conf(App) ->
    {ok, PgSize} = application:get_env(App, pg_size),
    {ok, PgHost} = application:get_env(App, pg_host),
    {ok, PgDatabase} = application:get_env(App, pg_database),
    {ok, PgUsername} = application:get_env(App, pg_username),
    {ok, PgPassword} = application:get_env(App, pg_password),
    [   {size, PgSize},
        {host, PgHost},
        {database, PgDatabase},
        {username, PgUsername},
        {password, PgPassword},
        {ssl, false}].

start(_StartType, _StartArgs) ->
    {ok, Pid} = keuli_sup:start_link(),
    PgConf = get_pg_conf(keuli),
    pgapp:connect(PgConf),
    Routes = [ {
        '_',
        [
            {"/static/[...]", keuli_static_handler, []},
            {"/favicon.ico", keuli_favicon_handler, []},
            {"/:username", keuli_user_handler, []},
            {"/", keuli_index_handler, []}
        ]
    } ],
    Dispatch = cowboy_router:compile(Routes),
    Port = os:getenv("PORT", "3000"),
    TransOpts = [ {ip, {0,0,0,0}}, {port, list_to_integer(Port)} ],
    ProtoOpts = #{ env => #{ dispatch => Dispatch } },
    %% Start listening over clear
    {ok, _} = cowboy:start_clear(keuli_app_clear, TransOpts, ProtoOpts),
    {ok, Pid}.

stop(_State) ->
    ok.

%% internal functions
