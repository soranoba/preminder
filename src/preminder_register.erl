%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(preminder_register).
-behaviour(websocket_client_handler).

-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'websocket_client_handler' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/2, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-record(?MODULE,
        {
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

start_link() ->
    case preminder_slack:rtm_start() of
        {ok, Url} ->
            websocket_client:start_link(binary_to_list(Url), ?MODULE, []);
        {error, Reason} ->
            {error, Reason}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(_, _) ->
    {ok, #?MODULE{}}.

%% @private
websocket_handle({text, Msg}, _, State) ->
    io:format("[handle] ~p~n", [Msg]),
    ok = preminder_msg_task:do(Msg),
    {ok, State};
websocket_handle(Msg, _, State) ->
    io:format("[handle] ~p~n", [Msg]),
    {ok, State}.

websocket_info(Msg, _, State) ->
    io:format("[info] ~p~n", [Msg]),
    {ok, State}.

websocket_terminate(Reason, _, _) ->
    io:format("[terminate] ~p~n", [Reason]),
    ok.
