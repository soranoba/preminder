%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(preminder_pr_tests).
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

compact_test_() ->
    [
     ?_assertEqual([{a, [1, 2, 3]}, {b, []}],
                   preminder_pr:compact([{a, 1}, b, {a, 2}, b, {a, 3}])),
     ?_assertEqual([{a, [1, 2, 3]}],
                   preminder_pr:compact([a, {a, 1}, a, {a, 2}, {a, 3}])),
     ?_assertEqual([{a, [1, 2, 3]}],
                   preminder_pr:compact([{a, 1}, a, {a, 2}, {a, 3}]))
    ].

fetch_urls_recursive_test_() ->
    [
     fun() ->
             Expected = [
                         <<"http://1">>,
                         <<"http://2">>
                        ],
             Inputs   = #{"hoge" => "fugo",
                          "urls" => [
                                     #{"url" => <<"http://1">>},
                                     #{"url" => <<"http://2">>}
                                    ]},
             ?assertEqual(lists:usort(Expected),
                          preminder_pr:fetch_urls_recursive(Inputs))
     end,
     fun() ->
             Expected = [
                         <<"http://1">>,
                         <<"http://2">>
                        ],
             Inputs   = #{"url"  => <<"http://1">>,
                          "hoge" => <<"http://dummy">>,
                          "urls" => [
                                     #{"url" => <<"http://2">>}
                                    ]},
             ?assertEqual(lists:usort(Expected),
                          preminder_pr:fetch_urls_recursive(Inputs))
     end,
     fun() ->
             Expected = [
                         <<"http://1">>,
                         <<"http://2">>,
                         <<"http://3">>,
                         <<"http://4">>,
                         <<"http://5">>,
                         <<"http://6">>
                        ],
             Inputs   = #{"hoge" => #{"urls" => [
                                                 #{"url" => <<"http://1">>}
                                                ]},
                          "fugo" => [
                                     #{"url"  => <<"http://2">>},
                                     #{"piyo" => #{"url" => <<"http://3">>}}
                                    ],
                          "urls" => [
                                     #{"url"  => <<"http://4">>},
                                     #{"urls" => [
                                                  #{"url" => <<"http://5">>},
                                                  #{"url" => <<"http://6">>}
                                                 ]}
                                    ]},
             ?assertEqual(lists:usort(Expected),
                          preminder_pr:fetch_urls_recursive(Inputs))
     end
    ].
