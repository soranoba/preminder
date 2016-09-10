%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Cross-convert the name of github/slack/real.

-module(preminder_user).
-behaviour(gen_server).

-include("preminder_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0,

         real_to_github/1,
         real_to_slack/1,

         github_to_real/1,
         github_to_slack/1,

         slack_to_real/1,
         slack_to_github/1,

         update/2
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
          real   :: undefined | binary(),
          github :: undefined | binary(),
          slack  :: undefined | binary()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc start and link the process.
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc real name to github name.
-spec real_to_github(binary()) -> {ok, binary()} | error.
real_to_github(RealName) ->
    select(RealName, #?MODULE.real, #?MODULE.github).

%% @doc real name to slack name.
-spec real_to_slack(binary()) -> {ok, binary()} | error.
real_to_slack(RealName) ->
    select(RealName, #?MODULE.real, #?MODULE.slack).

%% @doc github name to real name.
-spec github_to_real(binary()) -> {ok, binary()} | error.
github_to_real(GithubName) ->
    select(GithubName, #?MODULE.github, #?MODULE.real).

%% @doc github name to slack name.
-spec github_to_slack(binary()) -> {ok, binary()} | error.
github_to_slack(GithubName) ->
    select(GithubName, #?MODULE.github, #?MODULE.slack).

%% @doc slack name to real name.
-spec slack_to_real(binary()) -> {ok, binary()} | error.
slack_to_real(SlackName) ->
    select(SlackName, #?MODULE.slack, #?MODULE.real).

%% @doc slack name to github name.
-spec slack_to_github(binary()) -> {ok, binary()} | error.
slack_to_github(SlackName) ->
    select(SlackName, #?MODULE.slack, #?MODULE.github).

%% @doc Get the real name from `Name' that type is `NameType', and update cache.
-spec update(NameType, binary()) -> boolean() when
      NameType :: pos_integer() | github | slack.
update(Type, Name) when Type =:= #?MODULE.github; Type =:= github ->
    case preminder_github:real_name(Name) of
        {ok, RealName} ->
            gen_server:call(?MODULE, {update, #?MODULE{real = RealName, github = Name}});
        {error, Reason} ->
            _ = error_logger:warning_msg("[github api] ~p", [Reason]),
            false
    end;
update(Type, Name) when Type =:= #?MODULE.slack; Type =:= slack ->
    case preminder_slack:real_name(Name) of
        {ok, RealName} ->
            gen_server:call(?MODULE, {update, #?MODULE{real = RealName, slack = Name}});
        {error, Reason} ->
            _ = error_logger:warning_msg("[slack api] ~p", [Reason]),
            false
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(_) ->
    ?MODULE = ets:new(?MODULE, [named_table, protected, {keypos, #?MODULE.real}, {read_concurrency, true}]),
    {ok, ?MODULE}.

%% @private
handle_call({update, #?MODULE{real = RealName} = Object}, _From, State) when is_binary(RealName) ->
    Old = case ets:lookup(?MODULE, RealName) of
              []     -> #?MODULE{};
              [Old0] -> Old0
          end,
    true = ets:insert(?MODULE, merge_record(Object, Old)),
    {reply, true, State};
handle_call({update, _}, _, State) ->
    {reply, false, State};
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

%% @doc Search a record which `MatchPos'th element is `Match', and return the `GetPos'the element of the record.
%%
%% If it is not found, it challenge to update, and return error.
-spec select(binary(), pos_integer(), pos_integer()) -> {ok, binary()} | error.
select(Match, MatchPos, GetPos) ->
    Ms = ets:fun2ms(fun(Record) when element(MatchPos, Record) =:= Match -> element(GetPos, Record) end),
    case ets:select(?MODULE, Ms) of
        [Name] when is_binary(Name) -> {ok, Name};
        _ ->
            _ = update(MatchPos, Match),
            error
    end.

%% @doc It override to new value from old value, if new value is not `undefined'.
-spec merge_record(#?MODULE{}, #?MODULE{}) -> #?MODULE{}.
merge_record(New, Old) ->
    list_to_tuple(merge_record_impl(tuple_to_list(New), tuple_to_list(Old), [])).

%% @see merge_record/2
-spec merge_record_impl(New :: [Value], Old :: [Value], Acc :: [Value]) -> [Value] when
      Value :: binary() | undefined.
merge_record_impl([], [], Acc) ->
    lists:reverse(Acc);
merge_record_impl([X | Xs], [Y | Ys], Acc) when X =:= undefined ->
    merge_record_impl(Xs, Ys, [Y | Acc]);
merge_record_impl([X | Xs], [_ | Ys], Acc) ->
    merge_record_impl(Xs, Ys, [X | Acc]).
