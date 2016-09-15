%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Utility module.
%% @private

-module(preminder_util).
-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

-export([
         request/1,
         priv/1,
         uri_encode/1,
         get_env/1,
         is_match/2,
         is_match/3,
         partition_map/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc HTTP GET request that response is json.
-spec request(binary()) -> {ok, map() | list()} | {error, Reason :: term()}.
request(Url) ->
    case hackney:request(get, Url) of
        {error, Reason} -> {error, Reason};
        {ok, _, _, Ref} ->
            case hackney:body(Ref) of
                {error, Reason} -> {error, Reason};
                {ok, Body}      -> {ok, jsone:decode(Body)}
            end
    end.

%% @doc return the priv path.
-spec priv(file:filename()) -> file:filename().
priv(RelativePath) ->
    case code:priv_dir(?APP) of
        {error, bad_name} -> error(not_found_application, [RelativePath]);
        Dir               -> filename:join(Dir, RelativePath)
    end.

%% @doc uri encode.
%%
%% NOTE: http_uri:encode presets + `\n'
-spec uri_encode(string()) -> string().
uri_encode(Str) ->
    List = [$;, $:, $@, $&, $=, $+, $,, $/, $?, $#, $[, $], $<, $>, $\", ${, $}, $|, $\\, $', $^, $%, $ ],
    lists:append([case lists:member(C, List) of
                      true -> [$% | integer_to_list(C, 16)];
                      false when C =:= $\n -> "%0A";
                      false -> [C]
                  end || C <- Str]).

%% @doc If `application:get_env/2' return undefined, it execute `os:env/1'
%%
%% NOTE: Type of Val MUST be a string.
%%       OS environment variable is automatically capitalized.
-spec get_env(atom()) -> {ok, string()} | undefined.
get_env(Par) ->
    case application:get_env(?APP, Par) of
        undefined ->
            case os:getenv(string:to_upper(atom_to_list(Par))) of
                false -> undefined;
                Val   -> {ok, Val}
            end;
        {ok, Val} when is_list(Val) ->
            {ok, Val};
        {ok, Val} ->
            error({invalid_env, Val}, [Par])
    end.

%% @doc It return boolean that result of `re:run'.
-spec is_match(binary(), binary()) -> boolean().
is_match(Bin, Pattern) ->
    re:run(Bin, Pattern) =/= nomatch.

%% @doc It return boolean that result of `re:run'.
-spec is_match(binary(), binary(), Options :: [term()]) -> boolean().
is_match(Bin, Pattern, Options) ->
    re:run(Bin, Pattern, Options) =/= nomatch.

%% @doc `lists:partition/2' and `lists:map/2'
-spec partition_map(Pred, [term()]) -> {Satisfying :: [term()], NotSatisfying :: term()} when
      Pred :: fun((term()) -> {boolean(), term()}).
partition_map(Pred, List) ->
    partition_map(Pred, List, [], []).

%% @see partition_map/2
-spec partition_map(Pred, [term()], [term()], [term()]) -> {[term()], [term()]} when
      Pred :: fun((term()) -> {boolean(), term()}).
partition_map(_Pred, [], Satis, NotSatis) ->
    {lists:reverse(Satis), lists:reverse(NotSatis)};
partition_map(Pred, [X | Xs], Satis, NotSatis) ->
    case Pred(X) of
        {true, V}  -> partition_map(Pred, Xs, [V | Satis], NotSatis);
        {false, V} -> partition_map(Pred, Xs, Satis, [V | NotSatis])
    end.
