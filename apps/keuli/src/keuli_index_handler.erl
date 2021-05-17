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
        <title>"/utf8>>,
        Title,
        <<"</title>
        <!--<script src=\"/static/app.js\" type=\"text/javascript\"></script>-->
        <!--<link href=\"/static/font.css\" rel=\"stylesheet\">-->
        <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/css/style.css\" />
        <script type=\"text/javascript\">
            window.appData = "/utf8>>,
        EscapedData,
        <<";
        </script>
        <style>
        @import url('https://fonts.googleapis.com/css2?family=Archivo+Black&family=Black+Han+Sans&display=swap');

        .typemark {
            font-family: 'Archivo Black', sans-serif;
            font-weight: 400;
        }
        .typemark_kr {
            font-family: 'Black Han Sans', sans-serif;
            font-weight: 400;
        }
        </style>
    </head>
    <body>
        <div id=\"app\">
            <h1><span class=\"typemark_kr\">크리</span> <span class=\"typemark\">keu.li</span></h1>
            <div>The One Link to Rule Them All</div>
        </div>
    </body>
</html>"/utf8>>
    ].


init(Req, Opts) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        render_index_page(
            <<"크리 keu.li | One Link to Rule Them All"/utf8>>,
            [{"foo", <<"bar">>}]),
        Req),
    {ok, Req2, Opts}.
