-module(keuli_favicon_handler).

-import(keuli_app, [read_priv_file/1]).
-export([init/2]).

init(Req, Opts) ->
    Req2 = case read_priv_file("favicon/favicon.ico") of
        {ok, File} ->
            cowboy_req:reply(200,
                #{<<"content-type">> => <<"image/x-icon">>},
                File,
                Req);
        {error, _} ->
            cowboy_req:reply(404,
                #{<<"content-type">> => <<"text/plain">>},
                <<"File not found">>,
                Req)
    end,
    {ok, Req2, Opts}.
