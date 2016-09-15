%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(preminder_util_tests).
-include_lib("eunit/include/eunit.hrl").

-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

uri_encode_test_() ->
    [
      fun() ->
              In  = ";:@&=+,/?#[]<>\"{}|\'^%\\ \n",
              Bin = list_to_binary(preminder_util:uri_encode(In)),
              Zip = lists:zip(binary:split(Bin, <<"%">>, [global, trim_all]), In),
              [?assertEqual(lists:flatten(io_lib:format("~2.2.0s", [binary_to_list(Expected)])),
                            lists:flatten(io_lib:format("~2.2.0s", [integer_to_list(X, 16)])))
               || {Expected, X} <- Zip]
      end
    ].

get_env_test_() ->
    {foreach,
     fun() ->
             application:load(?APP),
             os:unsetenv("A")
     end,
     fun(_) ->
             os:unsetenv("A")
     end,
     [
      fun() ->
              application:set_env(?APP, a, "1"),
              ?assertEqual({ok, "1"}, preminder_util:get_env(a)),
              os:putenv("A", "2"),
              ?assertEqual({ok, "1"}, preminder_util:get_env(a))
      end,
      fun() ->
              os:putenv("A", "1"),
              ?assertEqual({ok, "1"}, preminder_util:get_env(a)),
              application:set_env(?APP, a, "2"),
              ?assertEqual({ok, "2"}, preminder_util:get_env(a))
      end
     ]}.

is_match_test_() ->
    [
     ?_assert(preminder_util:is_match(<<"hoge">>, <<".*">>)),
     ?_assertNot(preminder_util:is_match(<<"hoge">>, <<"fugo">>)),
     ?_assert(preminder_util:is_match(<<"hoge">>, <<"ge">>, [{offset, 2}])),
     ?_assertNot(preminder_util:is_match(<<"hoge">>, <<"ho">>, [{offset, 2}]))
    ].

partition_map_test_() ->
    [
     ?_assertEqual({[1, 3, 5], [4, 8, 12]},
                   preminder_util:partition_map(fun(X) ->
                                                        {X rem 2 =:= 1, X * ((X - 1) rem 2 + 1)}
                                                end, lists:seq(1, 6)))
    ].
