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
         request/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc HTTP GET request that response is json.
-spec request(binary()) -> {ok, map()} | {error, Reason :: term()}.
request(Url) ->
    case hackney:request(get, Url) of
        {error, Reason} -> {error, Reason};
        {ok, _, _, Ref} ->
            case hackney:body(Ref) of
                {error, Reason} -> {error, Reason};
                {ok, Body}      -> {ok, jsone:decode(Body)}
            end
    end.
