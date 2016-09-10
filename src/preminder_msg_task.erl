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
         task_github_url/1
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
do_1(#{<<"type">> := <<"message">>, <<"text">> := Text}) ->
    match_and_run(Text,
                  [
                   {<<"https?://[^\s]*(pull|issue)/[0-9]*">>, {?MODULE, task_github_url, []}}
                  ]);
do_1(_) ->
    ok.

match_and_run(_, []) ->
    ok;
match_and_run(Text, [{Pattern, {M, F, Args}} | Rest]) ->
    Matches = case re:run(Text, Pattern, [global]) of
                  {match, Matches0} -> Matches0;
                  nomatch           -> []
              end,
    lists:foreach(fun([{Start, Len} | _]) ->
                          catch apply(M, F, [binary:part(Text, Start, Len) | Args])
                  end, Matches),
    match_and_run(Text, Rest).

-spec task_github_url(binary()) -> ok.
task_github_url(GitHubUrl) ->
    [_, Host, Owner, Repos, _, Number] = binary:split(GitHubUrl, [<<"/">>, <<":">>], [global, trim_all]),
    ?IF(binary:match(preminder_github:endpoint(), Host) =:= nomatch, throw(return)),

    case preminder_github:pr(Owner, Repos, Number) of
        {ok, Body, open} ->
            Matches = case re:run(Body, <<"^\s*-\ \[\ \]\ @([^\s]*).*$">>, [global]) of
                          {match, Matches0} -> Matches0;
                          nomatch           -> []
                      end,
            Reals = lists:foldl(fun([_, {Start, Len}], Acc) ->
                                        Account = binary:part(Body, Start, Len),
                                        case preminder_user:github_to_real(Account) of
                                            {ok, Real} -> [Real | Acc];
                                            error      -> Acc
                                        end
                                end, [], Matches),
            preminder_pr:set(GitHubUrl, Reals);
        _ ->
            ok
    end.
