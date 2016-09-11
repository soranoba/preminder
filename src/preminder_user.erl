%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Cross-convert the name of github/slack/real.

-module(preminder_user).
-behaviour(gen_server).

-include("preminder_internal.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0,

         mail_to_github/1,
         mail_to_slack_id/1,
         mail_to_slack_name/1,

         github_to_mail/1,
         github_to_slack_id/1,
         github_to_slack_name/1,

         slack_id_to_mail/1,
         slack_id_to_github/1,
         slack_id_to_slack_name/1,

         slack_name_to_mail/1,
         slack_name_to_github/1,
         slack_name_to_slack_id/1,

         update/2,
         insert_github/1
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
          mail       :: undefined | binary(),
          github     :: undefined | binary(),
          slack_id   :: undefined | binary(),
          slack_name :: undefined | binary()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc start and link the process.
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc mail to github name.
-spec mail_to_github(binary()) -> {ok, binary()} | error.
mail_to_github(Mail) ->
    select(Mail, #?MODULE.mail, #?MODULE.github).

%% @doc mail to slack id.
-spec mail_to_slack_id(binary()) -> {ok, binary()} | error.
mail_to_slack_id(Mail) ->
    select(Mail, #?MODULE.mail, #?MODULE.slack_id).

%% @doc mail to slack name.
-spec mail_to_slack_name(binary()) -> {ok, binary()} | error.
mail_to_slack_name(Mail) ->
    select(Mail, #?MODULE.mail, #?MODULE.slack_name).

%% @doc github name to mail.
-spec github_to_mail(binary()) -> {ok, binary()} | error.
github_to_mail(GithubName) ->
    select(GithubName, #?MODULE.github, #?MODULE.mail).

%% @doc github name to slack id.
-spec github_to_slack_id(binary()) -> {ok, binary()} | error.
github_to_slack_id(GithubName) ->
    select(GithubName, #?MODULE.github, #?MODULE.slack_id).

%% @doc github name to slack name.
-spec github_to_slack_name(binary()) -> {ok, binary()} | error.
github_to_slack_name(GithubName) ->
    select(GithubName, #?MODULE.github, #?MODULE.slack_name).

%% @doc slack id to mail.
-spec slack_id_to_mail(binary()) -> {ok, binary()} | error.
slack_id_to_mail(SlackId) ->
    select(SlackId, #?MODULE.slack_id, #?MODULE.mail).

%% @doc slack id to github name.
-spec slack_id_to_github(binary()) -> {ok, binary()} | error.
slack_id_to_github(SlackId) ->
    select(SlackId, #?MODULE.slack_id, #?MODULE.github).

%% @doc slack id to slack name.
-spec slack_id_to_slack_name(binary()) -> {ok, binary()} | error.
slack_id_to_slack_name(SlackId) ->
    select(SlackId, #?MODULE.slack_id, #?MODULE.slack_name).

%% @doc slack name to mail.
-spec slack_name_to_mail(binary()) -> {ok, binary()} | error.
slack_name_to_mail(SlackName) ->
    select(SlackName, #?MODULE.slack_name, #?MODULE.mail).

%% @doc slack name to github name.
-spec slack_name_to_github(binary()) -> {ok, binary()} | error.
slack_name_to_github(SlackName) ->
    select(SlackName, #?MODULE.slack_name, #?MODULE.github).

%% @doc slack name to slack id.
-spec slack_name_to_slack_id(binary()) -> {ok, binary()} | error.
slack_name_to_slack_id(SlackName) ->
    select(SlackName, #?MODULE.slack_name, #?MODULE.slack_id).

%% @doc Get the mail name from `Name' that type is `NameType', and update cache.
-spec update(NameType, binary()) -> boolean() when
      NameType :: pos_integer() | github | slack.
update(Type, Name) when Type =:= #?MODULE.github; Type =:= github ->
    case preminder_github:mail(Name) of
        {ok, Mail} ->
            gen_server:call(?MODULE, {update, #?MODULE{mail = Mail, github = Name}});
        {error, Reason} ->
            _ = error_logger:warning_msg("[github api] mail(~p) -> ~p~n", [Name, Reason]),
            false
    end;
update(Type, SlackId) when Type =:= #?MODULE.slack_id; Type =:= slack ->
    case preminder_slack:user_info(SlackId) of
        {ok, {Mail, SlackName}} ->
            gen_server:call(?MODULE, {update, #?MODULE{mail = Mail, slack_id = SlackId, slack_name = SlackName}});
        {error, Reason} ->
            _ = error_logger:warning_msg("[slack api] mail(~p) -> ~p~n", [SlackId, Reason]),
            false
    end;
update(_, _) ->
    false.

-spec insert_github([{Mail :: binary(), GithubLoginId :: binary()}]) -> ok.
insert_github(List) ->
    lists:foreach(fun({Mail, LoginId}) ->
                          gen_server:call(?MODULE, {update, #?MODULE{mail = Mail, github = LoginId}})
                  end, List).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(_) ->
    case application:get_env(?APP, ?USER_DETS) of
        undefined      -> {stop, {?USER_DETS, not_found}};
        {ok, DetsFile} ->
            case dets:open_file(?MODULE, [{file, DetsFile}, {keypos, #?MODULE.mail}]) of
                {ok, ?MODULE}   -> {ok, ?MODULE};
                {error, Reason} -> {stop, Reason}
            end
    end.

%% @private
handle_call({update, #?MODULE{mail = Mail} = Object}, _From, State) when is_binary(Mail) ->
    Old = case dets:lookup(?MODULE, Mail) of
              []     -> #?MODULE{};
              [Old0] -> Old0
          end,
    case dets:insert(?MODULE, merge_record(Object, Old)) of
        ok              -> {reply, true, State};
        {error, Reason} -> {stop, Reason, false, State}
    end;
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
    case dets:select(?MODULE, Ms) of
        [Name] when is_binary(Name) -> {ok, Name};
        _ ->
            case update(MatchPos, Match) andalso dets:select(?MODULE, Ms) of
                [Name] when is_binary(Name) -> {ok, Name};
                _                           -> error
            end
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
