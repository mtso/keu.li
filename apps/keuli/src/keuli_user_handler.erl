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

init(Req, Opts) ->
    Username = cowboy_req:binding(username, Req),
    UserResult = get_user(Username),
    Req2 = case UserResult of
        {ok, User} -> cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, render_user_page(User), Req);
        {error, _} -> cowboy_req:reply(404, #{<<"content-type">> => <<"text/html">>}, render_404(Username), Req)
    end,
    {ok, Req2, Opts}.

%% internal functions

-record(font, {
    import_stmt,
    css_string,
    name}).

get_font(<<"archivo_black">>) -> #font{
    import_stmt = <<"@import url('https://fonts.googleapis.com/css2?family=Archivo+Black&display=swap');"/utf8>>,
    css_string = <<"font-family: 'Archivo Black', sans-serif;\nfont-weight: 400;"/utf8>>,
    name = <<"Archivo Black"/utf8>>};
get_font(<<"space_grotesk">>) -> #font{
    import_stmt = <<"@import url('https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@700&display=swap');"/utf8>>,
    css_string = <<"font-family: 'Space Grotesk', sans-serif;\nfont-weight: 700;"/utf8>>,
    name = <<"Space Grotesk"/utf8>>};
get_font(_Default) -> get_font(<<"archivo_black">>).

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

escape_url(Text) ->
    T0 = string:replace(Text, <<"\"">>, <<"%22">>, all),
    T1 = string:replace(T0, <<">">>, <<"%3E">>, all),
    string:replace(T1, <<"<">>, <<"%3C">>, all).

find_prop(Props, PropName, Default) ->
    FindProp = fun(A, Acc) -> case A of {PropName, Value} -> Value; _ -> Acc end end,
    lists:foldl(FindProp, Default, Props).

render_link(LinkProps) ->
    Url = find_prop(LinkProps, <<"url">>, <<"">>),
    Name = find_prop(LinkProps, <<"name">>, <<"">>),
    [
        <<"<div class=\"link\"><a href=\"">>,
        escape_url(Url),
        <<"\" target=\"_blank\">">>,
        Name,
        <<"</a></div>">>
    ].

render_links(JsonFields) ->
    {_, Fields} = JsonFields,
    Links = find_prop(Fields, <<"links">>, []),
    LinkEls = lists:map(fun(Link) -> {_, Props} = Link, render_link(Props) end, Links),
    [
        <<"<div class=\"links\">">>,
        LinkEls,
        <<"</div>">>
    ].

render_user_page(User) ->
    DisplayName = User#user.display_name,
    Title = [DisplayName, <<" | 크리 keu.li"/utf8>>],
    Links = render_links(User#user.fields),
    JsonString = mochijson2:encode([{"user", [{fields, User#user.fields}]}]),
    EscapedData = string:replace(JsonString, <<"<">>, <<"\\u003c">>, all),
    {_, FieldProps} = User#user.fields,
    Font = get_font(find_prop(FieldProps, <<"display_name_font">>, <<"">>)),
    DisplayNameFontImport = Font#font.import_stmt,
    DisplayNameFontStyleString = Font#font.css_string,
    [
        <<"<!DOCTYPE html>
<html>
    <head>
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
            "/utf8>>,
        DisplayNameFontImport,
        <<"

            .display_name {
                "/utf8>>,
        DisplayNameFontStyleString,
        <<"
            }
        </style>
    </head>
    <body>
        <div id=\"app\">
            <h1 class=\"display_name\">"/utf8>>,
        DisplayName,
        <<"</h1>">>,
        Links,
        <<"</div>
    </body>
</html>"/utf8>>
    ].

render_404(_) ->
    [
        <<"<!DOCTYPE html>
<html>
    <head>
        <meta charset=\"UTF-8\">
        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
        <title>크리 keu.li | Page Not Found</title>
        <!--<script src=\"/static/app.js\" type=\"text/javascript\"></script>-->
        <!--<link href=\"/static/font.css\" rel=\"stylesheet\">-->
        <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/css/style.css\" />
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
            <h1><span class=\"typemark_kr\">크리</span> <span class=\"typemark\">keu.li</span></h1>"/utf8>>,
        <<"<div>
            <h3>The page you're looking for does not exist.<div style=\"margin-top:0.3em\"><a href=\"/\">Return to the homepage</a></div></h3></div>"/utf8>>,
        <<"</div>
    </body>
</html>"/utf8>>
    ].
