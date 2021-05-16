-module(keuli_style_handler).

-export([init/2]).

read_priv_file(Filename) ->
    case code:priv_dir(keuli) of
        {error, bad_name} ->
            io:format("bad file name!"),
            PrivDir = "apps/keuli/priv";
        PrivDir -> ok
    end,
    file:read_file(filename:join([PrivDir, Filename])).

init(Req, Opts) ->
    % {ok, _, [{Value}]} = pgapp:equery("select (now() at time zone 'utc') as time", []),
    % io:format("~s\n", [timestamp_to_string(Value)]),
    % Username = cowboy_req:binding(name, Req),
    % User = get_user(Username),
    % io:format("~p\n", [User]),
    {ok, StyleFile} = read_priv_file("css/style.css"),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/css">>},
        StyleFile,
        Req),
    {ok, Req2, Opts}.
