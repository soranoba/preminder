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
         uri_encode/1
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
        Dir               ->
            filename:join(Dir, RelativePath)
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
