%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc To save the status of the pull request.

-module(preminder_pr).
-behaviour(gen_server).

-include("preminder_internal.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0,

         update/2,
         list/0,
         list/1
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
          mail   = '_' :: '_' | binary(),
          pr_url = '_' :: '_' | binary()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc start and link the process.
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc update the pull request information.
-spec update(binary(), [binary()]) -> ok.
update(Url, Mails) ->
    gen_server:call(?MODULE, {update, Url, Mails}).

%% @doc list of pull request informations.
-spec list() -> bbmuatache:data().
list() ->
    case dets:select(?MODULE, ets:fun2ms(fun(#?MODULE{mail = Mail, pr_url = Url}) -> {Url, Mail} end)) of
        {error, Reason} -> error(Reason, []);
        Other           ->
            #{"urls" => [#{"url" => U,
                           "users" => [#{"user" => S} || R <- Rs, is_binary(S = to_slack(R))]}
                         || {U, Rs} <- compact(Other)]}
    end.

%% @doc list of pull request informations that summarized in the user.
-spec list([binary()]) -> bbmustache:data().
list(Users0) ->
    Users = [from_slack(User) || User <- Users0],
    Ret = dets:foldl(fun(#?MODULE{mail = Mail, pr_url = Url}, Acc) ->
                             case lists:member(Mail, Users) of
                                 true  -> [{Mail, Url} | Acc];
                                 false -> Acc
                             end
                     end, [], ?MODULE),
    case Ret of
        {error, Reason} -> error(Reason, [Users]);
        Other           ->
            #{"users" => [#{"user" => S,
                            "urls" => [#{"url" => U} || U <- Us]}
                          || {R, Us} <- compact(Other), is_binary(S = to_slack(R))]}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(_) ->
    case application:get_env(?APP, ?PR_DETS) of
        undefined      -> {stop, {?PR_DETS, not_found}};
        {ok, DetsFile} ->
            case dets:open_file(?MODULE, [{file, DetsFile}, {keypos, #?MODULE.mail}, {type, duplicate_bag}]) of
                {ok, ?MODULE}   -> {ok, ?MODULE};
                {error, Reason} -> {stop, Reason}
            end
    end.

%% @private
handle_call({update, Url, Mails}, _, State) ->
    ok = dets:match_delete(?MODULE, #?MODULE{pr_url = Url, _ = '_'}),
    ok = ?IIF(Mails =:= [], ok, dets:insert(?MODULE, [#?MODULE{mail = Mail, pr_url = Url} || Mail <- Mails])),
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

%% @doc merge values with the same key
-spec compact([{term(), term()}]) -> [{term(), [term()]}].
compact(Proplists) ->
    compact(lists:reverse(lists:keysort(1, Proplists)), []).

%% @see compact/1
-spec compact([{term(), term()}], [{term(), [term()]}]) -> [{term(), [term()]}].
compact([], Acc) ->
    Acc;
compact([{X, V1} | Xs], [{X, Values} | Acc]) ->
    compact(Xs, [{X, [V1 | Values]} | Acc]);
compact([{X, V1} | Xs], Acc) ->
    compact(Xs, [{X, [V1]} | Acc]).

%% @doc mail to slack id.
-spec to_slack(binary()) -> binary() | false.
to_slack(Mail) ->
    case preminder_user:mail_to_slack_id(Mail) of
        {ok, SlackId} -> SlackId;
        _             -> false
    end.

%% @doc `<@SlackId>' or slack name to mail.
-spec from_slack(binary()) -> binary() | false.
from_slack(<<"<@", Rest/binary>>) when byte_size(Rest) > 1 ->
    SlackId = binary:part(Rest, 0, byte_size(Rest) - 1),
    case preminder_user:slack_id_to_mail(SlackId) of
        {ok, Mail} -> Mail;
        _          -> false
    end;
from_slack(SlackName) ->
    case preminder_user:slack_name_to_mail(SlackName) of
        {ok, Mail} -> Mail;
        _          -> false
    end.
