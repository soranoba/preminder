%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(preminder_storage).
-behaviour(gen_server).

-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0,
         get/1,
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

%% @doc start the cache (dets) server.
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc get value of key.
-spec get(term()) -> term().
get(Key) ->
    case dets:lookup(?MODULE, Key) of
        [] ->
            error(not_found, [Key]);
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
    case application:get_env(?APP, dets_path) of
        undefined  -> {stop, undefined_dets_path};
        {ok, Path} ->
            case dets:open_file(?MODULE, [{file, Path}]) of
                {ok, ?MODULE}   -> {ok, #?MODULE{}};
                {error, Reason} -> {stop, Reason}
            end
    end.

%% @private
handle_call({set, Key, Value}, _From, State) ->
    true = dets:insert(?MODULE, {Key, Value}),
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
