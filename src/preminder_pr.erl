%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc To save the status of the pull request.

-module(preminder_pr).
-behaviour(gen_server).

-include("preminder_internal.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0,

         update/3,
         list/0,
         list/1,
         r_list/1,
         fetch_urls_recursive/1,
         title/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------------------------------------------------------
-record(?MODULE,
        {
          login_id = '_' :: '_' | binary(),
          pr_url   = '_' :: '_' | binary()
        }).

-record(pr_title,
        {
          pr_url = '_' :: '_' | binary(),
          title  = '_' :: '_' | binary()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc start and link the process.
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc update the pull request information.
-spec update(binary(), binary(), [binary()]) -> ok.
update(Title, Url, Accounts) ->
    gen_server:call(?MODULE, {update, Title, Url, Accounts}).

%% @doc list of pull request informations.
-spec list() -> bbmuatache:data().
list() ->
    case dets:select(?MODULE, ets:fun2ms(fun(#?MODULE{login_id = Account, pr_url = Url}) -> {Url, Account} end)) of
        {error, Reason} -> error(Reason, []);
        Other           ->
            #{"urls" => [#{"title" => title(Url),
                           "url" => Url,
                           "users" => [#{"user" => SlackUser}
                                       || Account <- Accounts, is_binary(SlackUser = to_slack(Account))]}
                         || {Url, Accounts} <- compact(Other)]}
    end.

%% @doc list of pull request informations that summarized in the user.
-spec list([binary()]) -> bbmustache:data().
list(SlackUsers) ->
    {Accounts, UnknownUsers} = preminder_util:partition_map(fun(SlackUser) ->
                                                                    case from_slack(SlackUser) of
                                                                        false   -> {false, SlackUser};
                                                                        Account -> {true, Account}
                                                                    end
                                                            end, SlackUsers),
    Ret = dets:foldl(fun(#?MODULE{login_id = Account, pr_url = Url}, Acc) ->
                             case lists:member(Account, Accounts) of
                                 true  -> [{Account, Url} | Acc];
                                 false -> Acc
                             end;
                        (_, Acc) -> Acc
                     end, [], ?MODULE),
    case Ret of
        {error, Reason} -> error(Reason, [SlackUsers]);
        Other           ->
            #{"users" => [#{"user" => SlackUser,
                            "urls" => [#{"title" => title(Url), "url" => Url} || Url <- Urls]}
                          || {Account, Urls} <- compact(Other ++ Accounts), is_binary(SlackUser = to_slack(Account))],
              "unknowns" => [#{"user" => SlackUser} || SlackUser <- UnknownUsers]}
    end.

%% @doc List of reviewers of the pull request.
-spec r_list(binary()) -> bbmustache:data().
r_list(GithubUrls) ->
    Ret = dets:foldl(fun(#?MODULE{login_id = Account, pr_url = Url}, Acc) ->
                             case lists:member(Url, GithubUrls) of
                                 true  -> [{Url, Account} | Acc];
                                 false -> Acc
                             end;
                        (_, Acc) -> Acc
                     end, [], ?MODULE),
    case Ret of
        {error, Reason} -> error(Reason, [GithubUrls]);
        Other           ->
            #{"urls" => [#{"title" => title(Url),
                           "url" => Url,
                           "users" => [#{"user" => SlackUser}
                                       || Account <- Accounts, is_binary(SlackUser = to_slack(Account))]}
                         || {Url, Accounts} <- compact(Other)]}
    end.

%% @doc return the title of pull request.
-spec title(binary()) -> binary().
title(Url) ->
    case dets:lookup(?MODULE, Url) of
        {error, Reason} ->
            error(Reason, [Url]);
        [#pr_title{title = Title}] ->
            Title;
        _ ->
            <<"">>
    end.

%% @doc fetch urls from `bbmustache:data/0'
-spec fetch_urls_recursive(map()) -> [Url :: binary()].
fetch_urls_recursive(Map) ->
    lists:usort(maps:fold(fun fetch_urls_recursive_impl/3, [], Map)).

%% @see fetch_urls_recursive_impl/3
-spec fetch_urls_recursive_impl(term(), binary(), [binary()]) -> [binary()].
fetch_urls_recursive_impl("url",  Url,  Acc) ->
    [Url | Acc];
fetch_urls_recursive_impl(_, Map, Acc) when is_map(Map) ->
    maps:fold(fun fetch_urls_recursive_impl/3, Acc, Map);
fetch_urls_recursive_impl(_, List, Acc) when is_list(List) ->
    lists:foldl(fun(V, AccIn) ->
                        fetch_urls_recursive_impl('_', V, AccIn)
                end, Acc, List);
fetch_urls_recursive_impl(_, _, Acc) ->
    Acc.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(_) ->
    case preminder_util:get_env(?PR_DETS) of
        undefined      -> {stop, {?PR_DETS, not_found}};
        {ok, DetsFile} ->
            case dets:open_file(?MODULE, [{file, DetsFile}, {keypos, #?MODULE.login_id}, {type, duplicate_bag}]) of
                {ok, ?MODULE}   -> {ok, ?MODULE};
                {error, Reason} -> {stop, Reason}
            end
    end.

%% @private
handle_call({update, Title, Url, LoginIds}, _, State) ->
    ok = dets:match_delete(?MODULE, #?MODULE{pr_url = Url, _ = '_'}),
    _  = dets:delete(?MODULE, Url),
    _  = ?IIF(LoginIds =:= [], ok,
              begin
                  dets:insert(?MODULE, [#?MODULE{login_id = LoginId, pr_url = Url} || LoginId <- LoginIds]),
                  dets:insert(?MODULE, [#pr_title{pr_url = Url, title = Title}])
              end),
    {reply, ok, State};
handle_call(_, _, State) ->
    {noreply, State}.

%% @private
handle_cast(_, State) ->
    {noreply, State}.

%% @private
handle_info(_, State) ->
    {noreply, State}.

%% @private
code_change(_, State, _) ->
    {ok, State}.

%% @private
terminate(_, _) ->
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc merge values with the same key
-spec compact([{term(), term()} | term()]) -> [{term(), [term()]}].
compact(Proplists0) ->
    Proplists = lists:map(fun({_, _} = X) -> X;
                             (X)          -> {X}
                          end, Proplists0),
    compact(lists:reverse(lists:keysort(1, Proplists)), []).

%% @see compact/1
-spec compact([{term(), term()} | {term()}], [{term(), [term()]}]) -> [{term(), [term()]}].
compact([], Acc) ->
    Acc;
compact([{X, V1} | Xs], [{X, Values} | Acc]) ->
    compact(Xs, [{X, [V1 | Values]} | Acc]);
compact([{X, V1} | Xs], Acc) ->
    compact(Xs, [{X, [V1]} | Acc]);
compact([{X} | Xs], [{X, _} | _] = Acc) ->
    compact(Xs, Acc);
compact([{X} | Xs], Acc) ->
    compact(Xs, [{X, []} | Acc]).

%% @doc login id to slack id.
-spec to_slack(binary()) -> binary() | false.
to_slack(LoginId) ->
    case preminder_user:github_to_slack_id(LoginId) of
        {ok, SlackId} -> SlackId;
        _             -> false
    end.

%% @doc `<@SlackId>' or slack name to login id.
-spec from_slack(binary()) -> binary() | false.
from_slack(SlackNameOrMention) ->
    case preminder_slack:to_slack_id(SlackNameOrMention) of
        false   -> false;
        SlackId ->
            case preminder_user:slack_id_to_github(SlackId) of
                {ok, LoginId} -> LoginId;
                _             -> false
            end
    end.
