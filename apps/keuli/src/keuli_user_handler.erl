-module(keuli_user_handler).

-import(lists,[nth/2]).
-export([init/2]).

-define(USER_SELECT_QUERY,
    "SELECT id, username, display_name, create_time, modify_time,
            email, fields
     FROM users WHERE username = $1").

-record(user, {
  id,
  create_time,
  modify_time,
  username,
  display_name,
  email,
  fields}).

parse_user(Row) ->
    {Id, Username, DisplayName, CreateTime, ModifyTime, Email, Fields} = Row,
    #user{id=Id,
          username=Username,
          display_name=DisplayName,
          create_time=timestamp_to_string(CreateTime),
          modify_time=timestamp_to_string(ModifyTime),
          email=Email,
          fields=mochijson2:decode(Fields)}.

get_user(Username) ->
    Ref = pgapp:equery(?USER_SELECT_QUERY, [ Username ]),
    case Ref of
        {ok, _, [Row]} -> {ok, parse_user(Row)};
        {ok, _, []} -> {error, #user{}};
        {error, _, _} -> {error, #user{}}
    end.

timestamp_to_string(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Sec}} = Timestamp,
    list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~wZ",
        [Year, Month, Day, Hour, Minute, Sec])).

get_link(A, Acc) ->
    case {A, Acc} of
        {{<<"url">>, Url}, _} -> Url;
        _ -> Acc
    end.

get_name(A, Acc) ->
    case {A, Acc} of
        {{<<"name">>, Name}, _} -> Name;
        _ -> Acc
    end.

escape_url(Text) ->
    T0 = string:replace(Text, <<"\"">>, <<"%22">>, all),
    T1 = string:replace(T0, <<">">>, <<"%3E">>, all),
    string:replace(T1, <<"<">>, <<"%3C">>, all).

render_link(LinkProps) ->
    Url = lists:foldl(fun(A, B) -> get_link(A, B) end, <<"">>, LinkProps),
    Name = lists:foldl(fun(A, B) -> get_name(A, B) end, <<"">>, LinkProps),
    [
        <<"<div class=\"link\"><a href=\"">>,
        escape_url(Url),
        <<"\" target=\"_blank\">">>,
        Name,
        <<"</a></div>">>
    ].

render_links(JsonFields) ->
    {_, Fields} = JsonFields,
    Links = lists:foldl(
        fun(A,Ac) -> case {A,Ac} of {{<<"links">>, V}, _} -> V; _ -> Ac end end,
        [],
        Fields),
    LinkEls = lists:map(fun(Link) -> {_, Props} = Link, render_link(Props) end, Links),
    [
        <<"<div class=\"links\">">>,
        LinkEls,
        <<"</div>">>
    ].

render_user_page(User) ->
    DisplayName = User#user.display_name,
    Title = [DisplayName, <<" | keu.li">>],
    Links = render_links(User#user.fields),
    JsonString = mochijson2:encode([{"user", [{fields, User#user.fields}]}]),
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
            <h1>">>,
        DisplayName,
        <<"</h1>">>,
        Links,
        <<"</div>
    </body>
</html>">>
    ].

render_404(Username) ->
    [
        <<"<!DOCTYPE html>
<html>
    <head>
        <meta charset=\"UTF-8\">
        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
        <title>keu.li | Page Not Found</title>
        <!--<script src=\"/static/app.js\" type=\"text/javascript\"></script>-->
        <!--<link href=\"/static/font.css\" rel=\"stylesheet\">-->
        <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/style.css\" />
        <script type=\"text/javascript\">
        </script>
    </head>
    <body>
        <div id=\"app\">
            <h1>keu.li</h1>">>,
            <<"<div>
            <h3>The page you're looking for does not exist.</h3><a href=\"/\">Return to the homepage</a></div>">>,
        <<"</div>
    </body>
</html>">>
    ].

init(Req, Opts) ->
    Username = cowboy_req:binding(username, Req),
    UserResult = get_user(Username),
    Req2 = case UserResult of
        {ok, User} -> cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, render_user_page(User), Req);
        {error, _} -> cowboy_req:reply(404, #{<<"content-type">> => <<"text/html">>}, render_404(Username), Req)
    end,
    {ok, Req2, Opts}.
