%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(preminder_cache).
-behaviour(gen_server).

-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0,
         get/1,
         get/2,
         set/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------------------------------------------------------
-record(?MODULE,
        {
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc start the cache (ets) server.
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc get value of key.
-spec get(term()) -> term().
get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            error(not_found, [Key]);
        [{Key, Value}] ->
            Value
    end.

%% @doc get value of key.
-spec get(term(), term()) -> term().
get(Key, Default) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            Default;
        [{Key, Value}] ->
            Value
    end.

%% @doc insert the key-value.
-spec set(term(), term()) -> ok.
set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(_) ->
    ?MODULE = ets:new(?MODULE, [named_table, protected]),
    {ok, #?MODULE{}}.

%% @private
handle_call({set, Key, Value}, _From, State) ->
    true = ets:insert(?MODULE, {Key, Value}),
    {reply, ok, State};
handle_call(_, _, State) ->
    {noreply, State}.

%% @private
handle_cast(_, State) ->
    {noreply, State}.

%% @private
handle_info(_, State) ->
    {noreply, State}.

%% @private
code_change(_, State, _) ->
    {ok, State}.

%% @private
terminate(_, _) ->
    ok.
