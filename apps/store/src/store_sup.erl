-module(store_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
 supervisor:start_link(?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 10, period => 5},
  {ok, { SupFlags, []}}.
