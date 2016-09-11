%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-define(APP, preminder).
%% application name

-define(IIF(Cond, T, F),
        case Cond of true -> T; false -> F end).
%% immediate if

-define(IF(Cond, T), Cond andalso T).
-define(NOT(Cond, F), NOT orelse F).

-define(PRIV(Path), preminder_util:priv(Path)).
%% return the priv/`Path'

-define(PR_DETS, pr_dets_path).
-define(USER_DETS, user_dets_path).
