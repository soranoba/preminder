%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(preminder_msg_task_tests).
-define(Mod, preminder_msg_task).

-include_lib("eunit/include/eunit.hrl").

matches_test_() ->
    [
     ?_assertEqual([[<<"hoge">>], [<<"hoge">>]],
                   ?Mod:matches(<<"hoge fugo hoge">>, <<"hoge">>)),
     ?_assertEqual([[<<"abcdefghi">>, <<"bcdefghi">>], [<<"aaa">>, <<"aa">>]],
                   ?Mod:matches(<<"abcdefghi aaa">>, <<"a([^\s]+)">>)),
     ?_assertEqual([],
                   ?Mod:matches(<<"abcdefg">>, <<"\s">>))
    ].
