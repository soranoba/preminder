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
         mail/1,
         pr/3,
         fetch_mails/3
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc return the github end point.
-spec endpoint() -> binary().
endpoint() ->
    case preminder_util:get_env(github_endpoint) of
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
    case preminder_util:get_env(github_token) of
        {ok, Token} ->
            Token;
        undefined ->
            error(github_token_is_not_found, [])
    end.

%% @doc Generate a url from method and query parameters.
-spec url(string(), [{string(), string()}]) -> binary().
url(GithubMethod, QueryParams) ->
    QP = string:join([K ++ "=" ++ preminder_util:uri_encode(V) || {K, V} <- QueryParams], "&"),
    iolist_to_binary([endpoint(), GithubMethod, ?IIF(QP =:= [], QP, ["?", QP])]).

%% @doc get the mail.
-spec mail(binary()) -> {ok, binary()} | {error, Reason :: term()}.
mail(LoginId) ->
    Url = url("users/" ++ binary_to_list(LoginId), [{"token", token()}]),
    case preminder_util:request(Url) of
        {ok, #{<<"email">> := Mail}} ->
            {ok, Mail};
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

%% @doc fetch mails
-spec fetch_mails(binary(), binary(), binary()) -> {ok, [{Mail :: binary(), LoginId :: binary()}]} | {error, Reason :: term()}.
fetch_mails(Owner, Repos, Number) ->
    Url = url(binary_to_list(<<"repos/", Owner/binary, "/", Repos/binary, "/pulls/", Number/binary, "/commits">>),
              [{"token", token()}]),
    case preminder_util:request(Url) of
        {ok, #{<<"message">> := Message}} ->
            {error, Message};
        {ok, List} ->
            Ret = lists:map(fun(#{<<"commit">> := #{<<"author">> := #{<<"email">> := Mail}},
                                  <<"author">> := #{<<"login">> := LoginId}}) ->
                                    {Mail, LoginId}
                            end, List),
            {ok, lists:ukeysort(1, Ret)};
        {error, Reason} ->
            {error, Reason}
    end.
