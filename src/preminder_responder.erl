%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc It performs a process corresponding to the message.
-module(preminder_responder).

-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         do/1,

         %% tasks.
         task_github_url/1,
         task_list/2,
         task_register/3,
         task_user/2,
         task_help/1,
         task_remind/2,
         task_search/2,
         task_pray/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------

-type pattern() :: binary() | {mention, binary()}.
%% binary is regular expression.

-type task_in() :: {module(), Fun :: atom(), Args :: [term()]}.
%% Args support the some atoms.
%% ```
%% '$$' '$1' '$2' ...
%% '''
%% These atoms will automatically be replaced by the binary that matches.
%% e.g.
%% ```
%% Input   : <<"[[[ https://github.com/soranoba/preminder ]]]">>
%% Pattern : <<"https?://github.com/([^/\\s]*)/([^/\\s]*)">>
%%
%% $$ : <<"https://github.com/soranoba/preminder">>
%% $1 : <<"soranoba">>
%% $2 : <<"preminder">>
%% '''

-type task() :: {module(), Fun :: atom(), Args :: [term()]}.
%% Args is already replaced the Atoms. (e.g. $$, $1, ...)

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc It performs a process corresponding to the message.
-spec do(binary()) -> ok.
do(Msg) ->
    _ = spawn_link(
          fun() ->
                  try
                      do_1(jsone:decode(Msg))
                  catch
                      Class:Reason ->
                          error_logger:error_msg("~p:~p~n~p",
                                                 [Class, Reason, erlang:get_stacktrace()])
                  end
          end),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @see do/1
-spec do_1(map()) -> ok.
do_1(#{<<"attachments">> := [#{<<"pretext">> := PreText}]} = In) ->
    do_1(In#{<<"text">> => PreText, <<"attachments">> => nil});
do_1(#{<<"type">> := <<"message">>, <<"text">> := Text, <<"channel">> := Channel, <<"user">> := User}) ->
    Tasks0 = [
              {{mention, <<"remind.*">>},                           {?MODULE, task_remind,     [Text, Channel]}},
              {<<"https?://[^\\s]*(pull|issue)/[0-9]*">>,           {?MODULE, task_github_url, ['$$']}},
              {{mention, <<"list(.*)$">>},                          {?MODULE, task_list,       ['$1', Channel]}},
              {{mention, <<"register\\s+([^\\s]*)\\s+([^\\s]*)">>}, {?MODULE, task_register,   ['$1', '$2', Channel]}},
              {{mention, <<"user\\s+([^\\s]*)$">>},                 {?MODULE, task_user,       ['$1', Channel]}},
              {{mention, <<"help.*$">>},                            {?MODULE, task_help,       [Channel]}},
              {{mention, <<"search\\s(.*)$">>},                     {?MODULE, task_search,     ['$1', Channel]}},
              {{mention, <<":?pray:?.*$">>},                        {?MODULE, task_pray,       [User, Channel]}},
              {{mention, <<"^.*$">>},                               {?MODULE, task_help,       [Channel]}}
             ],
    ?NOT(User =:= preminder_slack:slack_id(),
         begin
             Tasks = choose_tasks(Text, Tasks0),
             execute_tasks(Tasks)
         end);
do_1(#{<<"type">> := <<"presence_change">>, <<"presence">> := <<"active">>, <<"user">> := SlackId}) ->
    %% NOTE: register or update the slack user.
    _ = preminder_user:slack_id_to_mail(SlackId),
    ok;
do_1(_) ->
    ok.


%% @doc choose the `[task_in()]' and convert the arguments if needed.
-spec choose_tasks(binary(), [{pattern(), task_in()}]) -> [task()].
choose_tasks(_Text, []) ->
    [];
choose_tasks(Text, [{{mention, Pattern}, TaskIn} | Rest]) ->
    SlackId = preminder_slack:slack_id(),
    case matches(Text, <<"<@", SlackId/binary, "(\|[^>]*)?>">>) of
        [] ->
            choose_tasks(Text, Rest);
        [[Mention|_] |_] ->
            case choose_tasks(binary:replace(Text, Mention, <<>>, [global]), [{Pattern, TaskIn}]) of
                []    -> choose_tasks(Text, Rest);
                Tasks -> Tasks
            end
    end;
choose_tasks(Text, [{Pattern, {M, F, Args0}} | Rest]) ->
    Matches = matches(Text, Pattern),
    case Matches of
        [] -> choose_tasks(Text, Rest);
        _  ->
            [begin
                 Args = lists:map(fun(MaybeAtom) ->
                                          case is_atom(MaybeAtom) andalso atom_to_list(MaybeAtom) of
                                              "$$"          -> lists:nth(1, Match);
                                              [$$ | IntStr] -> lists:nth(list_to_integer(IntStr) + 1, Match);
                                              _             -> MaybeAtom
                                          end
                                  end, Args0),
                 {M, F, Args}
             end || Match <- Matches]
    end.

%% @doc execute the tasks.
-spec execute_tasks([task()]) -> ok.
execute_tasks(Tasks) ->
    lists:foreach(fun({M, F, Args}) -> apply(M, F, Args) end, Tasks).

%% @doc preg_match and get the matching binaries.
-spec matches(binary(), binary()) -> [[binary()]].
matches(Text, Pattern) ->
    case re:run(Text, Pattern, [global]) of
        {match, MatchesList} -> [[binary:part(Text, Start, Len) || {Start, Len} <- Matches] || Matches <- MatchesList];
        nomatch              -> []
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Task Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc task for remind reviewers of the pull requests.
-spec task_remind(binary(), binary()) -> term().
task_remind(Text, Channel) ->
    Matches = matches(Text, <<"https?://[^\\s]*(pull|issue)/[0-9]*">>),
    Urls = [GithubUrl || [GithubUrl | _] <- Matches],
    ok  = preminder_util:pforeach(fun task_github_url/1, Urls),
    Ret = preminder_pr:r_list(Urls),
    preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("list_all.mustache")), Ret)).

%% @doc task for pull request url.
%%
%% NOTE: get or update the information of the pull request.
-spec task_github_url(binary()) -> term().
task_github_url(GitHubUrl) ->
    [_, Host, Owner, Repos, _, Number] = binary:split(GitHubUrl, [<<"/">>, <<":">>], [global, trim_all]),

    case binary:match(preminder_github:endpoint(), Host) =/= nomatch andalso preminder_github:pr(Owner, Repos, Number) of
        {ok, Body, open} ->
            Accounts = [Account || [_, _, Account] <- matches(Body, <<"(-|\\*)\\s*\\[\\s\\]\\s@([^\\s\\r\\n]*)\\s*">>)],
            _ = ?NOT(lists:all(fun(Account) -> preminder_user:github_to_mail(Account) =/= error end, Accounts),
                     case preminder_github:fetch_mails(Owner, Repos, Number) of
                         {ok, GithubInfos} ->
                             _ = preminder_user:insert_github(GithubInfos),
                             _ = error_logger:info_msg("[search commit] ~p~n", [GithubInfos]);
                         {error, _} ->
                             ok
                     end),
            _ = error_logger:info_msg("[recv github url] ~s -> ~p~n", [GitHubUrl, Accounts]),
            preminder_pr:update(GitHubUrl, Accounts);
        {ok, _, _} -> % not open.
            preminder_pr:update(GitHubUrl, []);
        _ ->
            ok
    end.

%% @doc task for list command.
%%
%% NOTE: get the list of the pull request that waiting review.
-spec task_list(binary(), binary()) -> term().
task_list(Text, Channel) ->
    Users = binary:split(Text, <<" ">>, [global, trim_all]),
    case lists:member(<<"all">>, Users) orelse Users =:= [] of
        true ->
            Ret0 = preminder_pr:list(),
            Urls = preminder_pr:fetch_urls_recursive(Ret0),
            ok   = preminder_util:pforeach(fun task_github_url/1, Urls),
            Ret  = preminder_pr:list(),
            preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("list_all.mustache")), Ret));
        false ->
            Ret0 = preminder_pr:list(Users),
            Urls = preminder_pr:fetch_urls_recursive(Ret0),
            ok   = preminder_util:pforeach(fun task_github_url/1, Urls),
            Ret  = preminder_pr:list(Users),
            preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("list.mustache")), Ret))
    end.

%% @doc task for register command.
%%
%% NOTE: register the github account and slack account pair.
-spec task_register(binary(), binary(), binary()) -> term().
task_register(SlackUser, GithubUser, Channel) ->
    case preminder_user:slack_name_to_mail(SlackUser) of
        {ok, Mail} ->
            ok  = preminder_user:insert_github([{Mail, GithubUser}]),
            task_user(SlackUser, Channel);
        error ->
            preminder_slack:post(Channel,
                                 iolist_to_binary(["[ERROR] ", SlackUser, " isn't register in databases..."]))
    end.

%% @doc task for user command.
%%
%% NOTE: show the user information.
-spec task_user(binary(), binary()) -> term().
task_user(SlackUser, Channel) ->
    case preminder_user:slack_name_to_mail(SlackUser) of
        {ok, Mail} ->
            Ret = preminder_user:lookup(Mail),
            preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("user.mustache")), Ret));
        error ->
            preminder_slack:post(Channel,
                                 iolist_to_binary(["[ERROR]", SlackUser, " isn't register in databases..."]))
    end.

%% @doc task for help command.
-spec task_help(binary()) -> term().
task_help(Channel) ->
    preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("help.mustache")), #{})).

%% @doc task for search command.
-spec task_search(binary(), binary()) -> term().
task_search(Query, Channel) ->
    Qs = binary:split(Query, <<" ">>, [global, trim_all]),
    SearchQuery = string:join([preminder_util:uri_encode(binary_to_list(Q)) || Q <- Qs], "+"),
    case preminder_github:pr_search(list_to_binary(SearchQuery)) of
        {ok, PullUrls} ->
            ok  = preminder_util:pforeach(fun task_github_url/1, PullUrls),
            Ret = preminder_pr:r_list(PullUrls),
            Msg = bbmustache:compile(bbmustache:parse_file(?PRIV("list_all.mustache")), Ret),
            preminder_slack:post(Channel, Msg);
        {error, Reason} ->
            preminder_slack:post(Channel, iolist_to_binary(io_lib:format("~p", [Reason])))
    end.

%% @doc task for pray command.
-spec task_pray(binary(), binary()) -> term().
task_pray(User, Channel) ->
    case preminder_user:slack_id_to_github(User) of
        {ok, GithubUser} ->
            task_search(<<"author:", GithubUser/binary>>, Channel);
        error ->
            preminder_slack:post(Channel, <<"[ERROR] Please register in this service">>)
    end.
