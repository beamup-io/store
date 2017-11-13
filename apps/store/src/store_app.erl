-module(store_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile(routes()),
  {ok, _} = cowboy:start_clear(api,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}),
  store_sup:start_link().

stop(_State) ->
  ok.

routes() ->
  [{'_', [{"/:project_name/release/:target_arch/:branch/:version",
           store_http_handler_release_single, []},
          {"/:project_name/release/:target_arch/:branch",
           store_http_handler_release_list, []}]}].
