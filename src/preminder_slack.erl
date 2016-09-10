%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Slack API

-module(preminder_slack).
-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         url/2,
         token/0,
         real_name/1,
         post/2,
         slack_id/0,
         rtm_start/0
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Defines
%%----------------------------------------------------------------------------------------------------------------------
-define(SLACK_USER_ID, slack_user_id).

-define(ENDPOINT, "https://slack.com/api/").
%% end point of slack api.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Generate a url from slack method and request parameters.
-spec url(string(), [{string(), string()}]) -> binary().
url(SlackMethod, QueryParams) ->
    QP = string:join([K ++ "=" ++ V || {K, V} <- QueryParams], "&"),
    iolist_to_binary([?ENDPOINT, SlackMethod, ?IIF(QP =:= [], QP, ["?", QP])]).

%% @doc get the token.
-spec token() -> string().
token() ->
    case application:get_env(?APP, slack_token) of
        {ok, Token} when is_list(Token) ->
            Token;
        {ok, _} ->
            error(slack_token_is_invalid_format, []);
        undefined ->
            error(slack_token_is_not_found, [])
    end.

%% @doc get the real name.
-spec real_name(SlackId :: binary()) -> {ok, binary()} | {error, Reason :: term()}.
real_name(SlackId) ->
    Url = url("users.info", [{"token", token()}, {"user", binary_to_list(SlackId)}]),
    case preminder_util:request(Url) of
        {ok, #{<<"user">> := #{<<"real_name">> := RealName}}} ->
            {ok, RealName};
        {ok, #{<<"error">> := Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc post the message.
-spec post(binary(), binary()) -> ok | {error, Reason :: term()}.
post(ChannelId, Msg) ->
    Url = url("chat.postMessage", [{"token", token()},
                                   {"channel", binary_to_list(ChannelId)},
                                   {"text", binary_to_list(Msg)}]),
    case preminder_util:request(Url) of
        {ok, #{<<"error">> := Reason}} ->
            {error, Reason};
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc return the slack user id.
-spec slack_id() -> binary().
slack_id() ->
    case preminder_cache:get(?SLACK_USER_ID, undefined) of
        undefined -> error(rtm_not_started, []);
        SlackId   -> SlackId
    end.

%% @doc start the rtm.
-spec rtm_start() -> {ok, Url :: binary()} | {error, Reason :: term()}.
rtm_start() ->
    case preminder_util:request(url("rtm.start", [{"token", token()}])) of
        {ok, #{<<"url">> := Url, <<"self">> := #{<<"id">> := MyId}}} ->
            ok = preminder_cache:set(?SLACK_USER_ID, MyId),
            {ok, Url};
        {ok, #{<<"error">> := Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.
