%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc preminder application.
%% @private

-module(preminder_app).
-behaviour(application).

-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback API
%%----------------------------------------------------------------------------------------------------------------------

-export([start/2, stop/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
start(_StartType, _StartArgs) ->
    Token = os:getenv("Token"),
    Path  = os:getenv("Path"),
    ok = application:set_env(?APP, token, Token),
    ok = application:set_env(?APP, dets_path, Path),
    preminder_sup:start_link().

stop(_State) ->
    ok.
