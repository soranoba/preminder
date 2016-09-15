%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc To perform the tasks corresponding to the slack message.
-module(preminder_msg_task).

-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         do/1
        ]).

-export([
         task_github_url/1,
         task_list/2,
         task_register/3,
         task_user/2,
         task_help/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc To perform the tasks corresponding to the slack message.
-spec do(binary()) -> ok.
do(Msg) ->
    _ = spawn_link(fun() ->
                           try
                               do_1(jsone:decode(Msg))
                           catch
                               Class:Reason ->
                                   error_logger:error_msg("~p:~p~n~p", [Class, Reason, erlang:get_stacktrace()])
                           end
                   end),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec do_1(map()) -> ok.
do_1(#{<<"attachments">> := [#{<<"pretext">> := PreText}]} = In) ->
    do_1(In#{<<"text">> => PreText, <<"attachments">> => nil});
do_1(#{<<"type">> := <<"message">>, <<"text">> := Text, <<"channel">> := Channel}) ->
    match_and_run(Text,
                  [
                   {<<"https?://[^\s]*(pull|issue)/[0-9]*">>,         {?MODULE, task_github_url, ['$$']}},
                   {{mention, <<"list(.*)">>},                        {?MODULE, task_list,       ['$1', Channel]}},
                   {{mention, <<"register\s+([^\s]*)\s+([^\s]*)">>},  {?MODULE, task_register,   ['$1', '$2', Channel]}},
                   {{mention, <<"user\s+([^\s]*)">>},                 {?MODULE, task_user,       ['$1', Channel]}},
                   {{mention, <<"help">>},                            {?MODULE, task_help,       [Channel]}}
                  ]);
do_1(#{<<"type">> := <<"presence_change">>, <<"presence">> := <<"active">>, <<"user">> := SlackId}) ->
    _ = preminder_user:slack_id_to_mail(SlackId),
    ok;
do_1(_) ->
    ok.

match_and_run(_, []) ->
    ok;
match_and_run(Text, [{{mention, Pattern}, {M, F, Args}} | Rest]) ->
    SlackId = preminder_slack:slack_id(),
    case matches(Text, <<"<@", SlackId/binary, "(\|[^>]*)?>">>) of
        []            -> match_and_run(Text, Rest);
        [[Mention|_] | _] ->
            ok = match_and_run(binary:replace(Text, Mention, <<>>, [global]), [{Pattern, {M, F, Args}}]),
            match_and_run(Text, Rest)
    end;
match_and_run(Text, [{Pattern, {M, F, Args0}} | Rest]) ->
    Matches = matches(Text, Pattern),
    lists:foreach(
      fun(Match) ->
              try
                  Args = lists:map(fun(Atom) when is_atom(Atom) ->
                                           case atom_to_list(Atom) of
                                               "$$"          -> lists:nth(1, Match);
                                               [$$ | IntStr] -> lists:nth(list_to_integer(IntStr) + 1, Match);
                                               _             -> Atom
                                           end;
                                      (Other) ->
                                           Other
                                   end, Args0),
                  apply(M, F, Args)
              catch
                  _:Reason ->
                      error_logger:error_msg("~p~n~p~n", [Reason, erlang:get_stacktrace()])
              end
      end, Matches),
    match_and_run(Text, Rest).

-spec matches(binary(), binary()) -> [[binary()]].
matches(Text, Pattern) ->
    case re:run(Text, Pattern, [global]) of
        {match, MatchesList} -> [[binary:part(Text, Start, Len) || {Start, Len} <- Matches] || Matches <- MatchesList];
        nomatch              -> []
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Task Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec task_github_url(binary()) -> ok.
task_github_url(GitHubUrl) ->
    [_, Host, Owner, Repos, _, Number] = binary:split(GitHubUrl, [<<"/">>, <<":">>], [global, trim_all]),
    ?IF(binary:match(preminder_github:endpoint(), Host) =:= nomatch, throw(return)),

    case preminder_github:pr(Owner, Repos, Number) of
        {ok, Body, open} ->
            Accounts = [Account || [_, _, Account] <- matches(Body, <<"(-|\\*)\s*\\[\s\\]\s@([^\s\\r\\n]*)\s*">>)],
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

task_list(Text, Channel) ->
    Users = binary:split(Text, <<" ">>, [global, trim_all]),
    case lists:member(<<"all">>, Users) orelse Users =:= [] of
        true ->
            Ret = preminder_pr:list(),
            preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("list_all.mustache")), Ret));
        false ->
            Ret = preminder_pr:list(Users),
            preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("list.mustache")), Ret))
    end.

task_register(SlackUser, GithubUser, Channel) ->
    case preminder_user:slack_name_to_mail(SlackUser) of
        {ok, Mail} ->
            ok  = preminder_user:insert_github([{Mail, GithubUser}]),
            task_user(SlackUser, Channel);
        error ->
            preminder_slack:post(Channel,
                                 iolist_to_binary(["[ERROR] ", SlackUser, " isn't register in databases..."]))
    end.

task_user(SlackUser, Channel) ->
    case preminder_user:slack_name_to_mail(SlackUser) of
        {ok, Mail} ->
            Ret = preminder_user:lookup(Mail),
            preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("user.mustache")), Ret));
        error ->
            preminder_slack:post(Channel,
                                 iolist_to_binary(["[ERROR]", SlackUser, " isn't register in databases..."]))
    end.

task_help(Channel) ->
    preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("help.mustache")), #{})).
