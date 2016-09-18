%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc websocket handler to communicate with the slack.

-module(preminder_ws).
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
    {ok, #?MODULE{}, 10000}.

%% @private
websocket_handle({text, Msg}, _, State) ->
    ok = preminder_responder:do(Msg),
    {ok, State};
websocket_handle({ping, _Msg}, _, State) ->
    {reply, {pong, <<>>}, State};
websocket_handle({pong, _Msg}, _, State) ->
    {ok, State};
websocket_handle(_Msg, _, State) ->
    {ok, State}.

websocket_info(_Msg, _, State) ->
    {ok, State}.

websocket_terminate(Reason, _, _) ->
    ok = error_logger:warning_msg("websocket terminated. reason : ~p~n", [Reason]),
    ok.
