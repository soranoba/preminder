%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Github API

-module(preminder_github).
-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         endpoint/0,
         token/0,
         url/2,
         real_name/1,
         pr/3
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc return the github end point.
-spec endpoint() -> binary().
endpoint() ->
    case application:get_env(?APP, github_endpoint) of
        {ok, [_|_] = EndPoint} ->
            list_to_binary(?IIF(lists:last(EndPoint) =:= $/, EndPoint, EndPoint ++ "/"));
        {ok, _} ->
            error(github_endpoint_is_invalid_format, []);
        undefined ->
            error(github_endpoint_is_not_found, [])
    end.

%% @doc return the github token.
-spec token() -> string().
token() ->
    case application:get_env(?APP, github_token) of
        {ok, Token} when is_list(Token) ->
            Token;
        {ok, _} ->
            error(github_token_is_invalid_format, []);
        undefined ->
            error(github_token_is_not_found, [])
    end.

%% @doc Generate a url from method and query parameters.
-spec url(string(), [{string(), string()}]) -> binary().
url(GithubMethod, QueryParams) ->
    QP = string:join([K ++ "=" ++ V || {K, V} <- QueryParams], "&"),
    iolist_to_binary([endpoint(), GithubMethod, ?IIF(QP =:= [], QP, ["?", QP])]).

%% @doc get the real name.
-spec real_name(binary()) -> {ok, binary()} | {error, Reason :: term()}.
real_name(LoginId) ->
    Url = url("users/" ++ binary_to_list(LoginId), [{"token", token()}]),
    case preminder_util:request(Url) of
        {ok, #{<<"name">> := Name}} ->
            {ok, Name};
        {ok, #{<<"message">> := Message}} ->
            {error, Message};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc get the pull request information.
-spec pr(binary(), binary(), binary()) -> {ok, Body :: binary(), open | closed} | {error, Reason :: term()}.
pr(Owner, Repos, Number) ->
    Url = url(binary_to_list(<<"repos/", Owner/binary, "/", Repos/binary, "/pulls/", Number/binary>>),
              [{"token", token()}]),
    case preminder_util:request(Url) of
        {ok, #{<<"body">> := Body, <<"closed_at">> := ClosedAt, <<"merged_at">> := MergedAt}} ->
            {ok, Body, ?IIF(ClosedAt =:= null andalso MergedAt =:= null, open, closed)};
        {ok, #{<<"message">> := Message}} ->
            {error, Message};
        {error, Reason} ->
            {error, Reason}
    end.
