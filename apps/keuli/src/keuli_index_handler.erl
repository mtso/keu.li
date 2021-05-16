-module(keuli_index_handler).

-export([init/2]).

render_index_page(Title, Params) ->
    JsonString = mochijson2:encode(Params),
    EscapedData = string:replace(JsonString, <<"<">>, <<"\\u003c">>, all),
    [
        <<"<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv='Content-Type' content='text/html;charset=utf-8'/>
        <meta charset=\"UTF-8\">
        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
        <title>">>,
        Title,
        <<"</title>
        <!--<script src=\"/static/app.js\" type=\"text/javascript\"></script>-->
        <!--<link href=\"/static/font.css\" rel=\"stylesheet\">-->
        <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/style.css\" />
        <script type=\"text/javascript\">
            window.appData = ">>,
        EscapedData,
        <<";
        </script>
    </head>
    <body>
        <div id=\"app\">
            <h1>keu.li</h1>
            <div>Link Bios</div>
        </div>
    </body>
</html>">>
    ].


init(Req, Opts) ->
    % Username = cowboy_req:binding(username, Req),
    % User = get_user(Username),
    % Links = render_links(User#user.fields),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        render_index_page(
            <<"keu.li link ">>,
            [{"foo", <<"bar">>}]),
        Req),
    {ok, Req2, Opts}.
