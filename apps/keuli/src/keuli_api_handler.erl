-module(keuli_api_handler).

-export([init/2]).

-define(USER_SELECT_QUERY,
    "SELECT id, username, display_name, display_image_url, create_time, modify_time,
            email, email_is_verified, fields
     FROM users WHERE username = $1").
% CREATE TABLE IF NOT EXISTS users (
%   id SERIAL PRIMARY KEY,
%   create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
%   modify_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
%   username VARCHAR(255),
%   display_name VARCHAR(4095),
%   display_image_url VARCHAR(4095),
%   email VARCHAR(255),
%   email_is_verified BOOLEAN DEFAULT FALSE,
%   fields JSON,
%   UNIQUE(username)
-record(user, {
    id,
    create_time,
    modify_time,
    username,
    display_name,
    display_image_url,
    email,
    email_is_verified,
    fields}).

init(Req, Opts) ->
    Username = cowboy_req:binding(username, Req),
    UserResult = get_user(Username),
    Req2 = case UserResult of
        {ok, User} -> cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, render_user_json(User), Req);
        {error, _} -> cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, render_404_json(), Req)
    end,
    {ok, Req2, Opts}.

%% internal functions

%% Encode proplist to preserve ordering.
render_user_json(User) ->
    mochijson2:encode(
        [{id, User#user.id},
         {create_time, User#user.create_time},
         {modify_time, User#user.modify_time},
         {username, User#user.username},
         {display_name, User#user.display_name},
         {display_image_url, User#user.display_image_url},
         {fields, User#user.fields}]).
    % mochijson2:encode(#{
    %     id => User#user.id,
    %     display_name => User#user.display_name,
    %     display_image_url => User#user.display_image_url,
    %     create_time => User#user.create_time,
    %     modify_time => User#user.modify_time,
    %     username => User#user.username,
    %     fields => User#user.fields}).

render_404_json() ->
    mochijson2:encode([{"error", "Not found"}]).

parse_user(Row) ->
    {Id, Username, DisplayName, DisplayImageUrl, CreateTime, ModifyTime,
          Email, EmailIsVerified, Fields} = Row,
    #user{id=Id,
          username=Username,
          display_name=DisplayName,
          display_image_url=DisplayImageUrl,
          create_time=timestamp_to_string(CreateTime),
          modify_time=timestamp_to_string(ModifyTime),
          email=Email,
          email_is_verified=EmailIsVerified,
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
