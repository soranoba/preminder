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
