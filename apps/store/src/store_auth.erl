-module(store_auth).

-export([is_authorized/3]).

is_authorized(put, _ProjectName, <<"secret">>) -> true;
is_authorized(get, _ProjectName, <<"secret">>) -> true;
is_authorized(_, _, _) -> false.
