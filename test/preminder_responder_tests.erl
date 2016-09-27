%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(preminder_responder_tests).
-define(Mod, preminder_responder).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

matches_test_() ->
    [
     ?_assertEqual([[<<"hoge">>], [<<"hoge">>]],
                   ?Mod:matches(<<"hoge fugo hoge">>, <<"hoge">>)),
     ?_assertEqual([[<<"abcdefghi">>, <<"bcdefghi">>], [<<"aaa">>, <<"aa">>]],
                   ?Mod:matches(<<"abcdefghi aaa">>, <<"a([^\s]+)">>)),
     ?_assertEqual([],
                   ?Mod:matches(<<"abcdefg">>, <<"\s">>))
    ].

choose_tasks_test_() ->
    [
     ?_assertEqual([], ?Mod:choose_tasks(<<>>, [])),

     ?_assertEqual([{m, f, [a, <<"hoge">>, b]}],
                   ?Mod:choose_tasks(<<"hoge">>, [{<<"^.*$">>, {m, f, [a, '$$', b]}}])),
     ?_assertEqual([{m, f, [<<"{a b}">>, <<"a">>, <<"b">>]},
                    {m, f, [<<"{c d}">>, <<"c">>, <<"d">>]}],
                   ?Mod:choose_tasks(<<"{a b}, {c d}">>,
                                     [{<<"{([^\s]*)\s([^\s]*)}">>, {m, f, ['$$', '$1', '$2']}}])),
     ?_assertEqual([{m, f, [<<"ab">>]},
                    {m, f, [<<"cd">>]}],
                   ?Mod:choose_tasks(<<"ab\ncd">>, [{<<".+">>, {m, f, ['$$']}}])),
     ?_assertEqual([{m, f, [<<"ab">>]}],
                   ?Mod:choose_tasks(<<"ab">>, [{<<"^.*$">>, {m, f, ['$$']}},
                                                {<<"^.*$">>, {m, f, [a]}}]))
    ].

choose_task2_test_() ->
    SlackId = <<"SLACK_ID">>,
    {setup,
     fun() ->
             _  = meck:new(preminder_slack, [no_link]),
             ok = meck:expect(preminder_slack, slack_id, 0, SlackId)
     end,
     fun(_) ->
             _ = meck:unload()
     end,
     [
      ?_assertEqual([{m, f, [<<"hoge">>]}],
                    ?Mod:choose_tasks(<<"<@", SlackId/binary, ">hoge">>,
                                      [{{mention, <<"^.*$">>}, {m, f, ['$$']}}])),
      ?_assertEqual([{m, f, [<<"hoge">>]}],
                    ?Mod:choose_tasks(<<"<@", SlackId/binary, "|dummyName>hoge">>,
                                      [{{mention, <<"^.*$">>}, {m, f, ['$$']}}])),
      ?_assertEqual([{m, f, [<<"hogefugo">>]}],
                    ?Mod:choose_tasks(<<"hoge<@", SlackId/binary, ">fugo">>,
                                      [{{mention, <<"^.*$">>}, {m, f, ['$$']}}]))
     ]}.
