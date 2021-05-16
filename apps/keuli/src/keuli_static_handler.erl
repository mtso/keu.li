-module(keuli_static_handler).

-import(keuli_app, [read_priv_file/1]).
-export([init/2]).

get_mime_type(Filepath) ->
    case filename:extension(Filepath) of
        <<".css">> -> <<"text/css">>;
        <<".js">> -> <<"text/javascript">>;
        <<".woff2">> -> <<"font/woff2">>;
        <<".woff">> -> <<"font/woff">>;
        <<".ttf">> -> <<"font/ttf">>;
        <<".otf">> -> <<"font/otf">>;
        _ -> <<"text/plain">>
    end.

init(Req, Opts) ->
    Filepath = cowboy_req:path_info(Req),
    MimeType = get_mime_type(filename:join(Filepath)),
    Req2 = case read_priv_file(filename:join(Filepath)) of
        {ok, StyleFile} ->
            cowboy_req:reply(200,
                #{<<"content-type">> => MimeType},
                StyleFile,
                Req);
        {error, _} ->
            cowboy_req:reply(404,
                #{<<"content-type">> => <<"text/plain">>},
                <<"File not found">>,
                Req)
    end,
    {ok, Req2, Opts}.
