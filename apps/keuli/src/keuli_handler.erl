-module(keuli_handler).

-import(lists,[nth/2]).
-export([init/2]).


-record(user, {
  id,
  create_time,
  modify_time,
  username,
  display_name,
  email,
  fields}).

-define(USER_SELECT_QUERY, "SELECT id, username, display_name, create_time, modify_time, email, fields FROM users WHERE username = $1").

get_user(Username) ->
    {ok, _, [Value]} = pgapp:equery(?USER_SELECT_QUERY, [ Username ]),
    {Id, Username, DisplayName, CreateTime, ModifyTime, Email, Fields} = Value,
    #user{id=Id,
          username=Username,
          display_name=DisplayName,
          create_time=timestamp_to_string(CreateTime),
          modify_time=timestamp_to_string(ModifyTime),
          email=Email,
          fields=mochijson2:decode(Fields)}.
    % [{id, Id}, {username, Username}, {create_time, timestamp_to_string(CreateTime)},
    %  {modify_time, timestamp_to_string(ModifyTime)}, {email, Email}, {fields, mochijson2:decode(Fields)}].

timestamp_to_string(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Sec}} = Timestamp,
    list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~wZ",
        [Year, Month, Day, Hour, Minute, Sec])).

index(Title, Params) ->
    JsonString = mochijson2:encode(Params),
    EscapedData = string:replace(JsonString, <<"<">>, <<"\\u003c">>, all),
    [
        <<"<!DOCTYPE html>
<html>
    <head>
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
        hi
        </div>
    </body>
</html>">>
    ].


init(Req, Opts) ->
    Username = cowboy_req:binding(name, Req),
    User = get_user(Username),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        index(
            User#user.display_name,
            [{"user", [{fields, User#user.fields}]}]),
        Req),
    {ok, Req2, Opts}.
