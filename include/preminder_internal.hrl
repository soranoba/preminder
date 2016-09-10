%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-define(APP, preminder).
%% application name

-define(IIF(Cond, T, F),
        case Cond of true -> T; false -> F end).
%% immediate if

-define(IF(Cond, T), Cond andalso T).
-define(NOT(Cond, F), NOT orelse F).
