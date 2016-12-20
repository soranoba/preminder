%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Setting of preminder.

-module(preminder_setting).
-behaviour(gen_server).

-include("preminder_internal.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0,

         lookup/1,
         append_values/2,
         remove_values/2
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
          key    :: binary(),
          values :: [binary()]
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc start and link the process.
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc lookup values of key.
-spec lookup(binary()) -> [binary()].
lookup(Key) ->
    Objs = dets:lookup(?MODULE, Key),
    case Objs of
        [#?MODULE{values = Values}] -> Values;
        _                           -> []
    end.

%% @doc append values
-spec append_values(binary(), [binary()]) -> ok.
append_values(Key, Value) ->
    gen_server:call(?MODULE, {append, Key, Value}).

%% @doc remove values
-spec remove_values(binary(), [binary()]) -> ok.
remove_values(Key, Value) ->
    gen_server:call(?MODULE, {remove, Key, Value}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(_) ->
    case preminder_util:get_env(?SETTING_DETS) of
        undefined      -> {stop, {?SETTING_DETS, not_found}};
        {ok, DetsFile} ->
            case dets:open_file(?MODULE, [{file, DetsFile}, {keypos, #?MODULE.key}]) of
                {ok, ?MODULE}   -> {ok, ?MODULE};
                {error, Reason} -> {stop, Reason}
            end
    end.

%% @private
handle_call({append, Key, Value}, _, State) ->
    Values = lookup(Key),
    ok = dets:insert(?MODULE, #?MODULE{key = Key, values = lists:usort(Values ++ Value)}),
    {reply, ok, State};
handle_call({remove, Key, Value}, _, State) ->
    Values = lookup(Key),
    ok = dets:insert(?MODULE, #?MODULE{key = Key, values = Values -- Value}),
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

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
