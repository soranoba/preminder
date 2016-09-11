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
         task_mention/3
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc To perform the tasks corresponding to the slack message.
-spec do(binary()) -> ok.
do(Msg) ->
    _ = spawn_link(fun() -> catch do_1(jsone:decode(Msg)) end),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec do_1(map()) -> ok.
do_1(#{<<"attachments">> := [#{<<"pretext">> := PreText}]} = In) ->
    do_1(In#{<<"text">> => PreText, <<"attachments">> => nil});
do_1(#{<<"type">> := <<"message">>, <<"text">> := Text, <<"channel">> := Channel}) ->
    SlackId = preminder_slack:slack_id(),
    match_and_run(Text,
                  [
                   {<<"https?://[^\s]*(pull|issue)/[0-9]*">>, {?MODULE, task_github_url, []}},
                   {<<"<@", SlackId/binary, ">">>,            {?MODULE, task_mention,    [Channel, Text]}}
                  ]);
do_1(#{<<"type">> := <<"presence_change">>, <<"presence">> := <<"active">>, <<"user">> := SlackId}) ->
    _ = preminder_user:slack_id_to_mail(SlackId),
    ok;
do_1(_) ->
    ok.

match_and_run(_, []) ->
    ok;
match_and_run(Text, [{Pattern, {M, F, Args}} | Rest]) ->
    Matches = case re:run(Text, Pattern, [global]) of
                  {match, Matches0} -> Matches0;
                  nomatch           -> []
              end,
    io:format("[match] ~p~n", [Matches]),
    lists:foreach(fun([{Start, Len} | _]) ->
                          try
                              apply(M, F, [binary:part(Text, Start, Len) | Args])
                          catch
                              _:Reason ->
                                  io:format("~p~n~p~n", [Reason, erlang:get_stacktrace()])
                          end
                  end, Matches),
    match_and_run(Text, Rest).

-spec task_github_url(binary()) -> ok.
task_github_url(GitHubUrl) ->
    io:format("[find] ~s~n", [GitHubUrl]),
    [_, Host, Owner, Repos, _, Number] = binary:split(GitHubUrl, [<<"/">>, <<":">>], [global, trim_all]),
    ?IF(binary:match(preminder_github:endpoint(), Host) =:= nomatch, throw(return)),

    case preminder_github:pr(Owner, Repos, Number) of
        {ok, Body, open} ->
            Matches = case re:run(Body, <<"(-|\\*)\s*\\[\s\\]\s@([^\s\\r\\n]*)\s*">>, [global]) of
                          {match, Matches0} -> Matches0;
                          nomatch           -> []
                      end,
            io:format("matches : ~p~n", [Matches]),
            Mails = lists:foldl(fun([_, _, {Start, Len}], Acc) ->
                                        Account = binary:part(Body, Start, Len),
                                        case preminder_user:github_to_mail(Account) of
                                            {ok, Mail} -> [Mail | Acc];
                                            error      -> Acc
                                        end
                                end, [], Matches),
            io:format("mail : ~p~n", [Mails]),
            _ = case length(Mails) =/= length(Matches) of
                    true ->
                        case preminder_github:fetch_mails(Owner, Repos, Number) of
                            {ok, GithubInfos} ->
                                io:format("[github info] ~p~n", [GithubInfos]),
                                _ = error_logger:info_msg("[search commit] ~p~n~p~n", [GithubInfos, preminder_user:insert_github(GithubInfos)]);
                            {error, _}        -> ok
                        end;
                    false ->
                        ok
                end,

            _ = error_logger:info_msg("[recv github url] ~s -> ~p~n", [GitHubUrl, Mails]),
            preminder_pr:update(GitHubUrl, Mails);
        _ ->
            ok
    end.

task_mention(_, Channel, Text) ->
    _ = error_logger:info_msg("[recv mention] ~s", [Text]),
    case binary:match(Text, <<"list">>) of
        nomatch      -> ok;
        {Start, Len} ->
            RestText = binary:part(Text, Start + Len, byte_size(Text) - (Start + Len)),
            Users = binary:split(RestText, <<" ">>, [global]),
            case lists:member(<<"all">>, Users) orelse Users =:= [] of
                false ->
                    Ret = preminder_pr:list(Users),
                    preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("list.mustache")), Ret));
                true ->
                    Ret = preminder_pr:list(),
                    error_logger:info_msg("~p", [preminder_slack:post(Channel, bbmustache:compile(bbmustache:parse_file(?PRIV("list_all.mustache")), Ret))])
            end
    end.
