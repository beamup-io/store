-module(store_http_handler_release_list).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([to_etf/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"erlang">>, []}, to_etf}], Req, State}.

resource_exists(Req, _State) ->
  ProjectName = cowboy_req:binding(project_name, Req),
  Architecture = cowboy_req:binding(architecture, Req),
  Branch = cowboy_req:binding(branch, Req),
  State = store_project:new(#{project_name => ProjectName,
                              architecture => Architecture,
                              branch => Branch}),
  case store_project:exists(State) of
    true -> {true, Req, State};
    false -> {false, Req, State}
  end.

to_etf(Req, State) ->
  Versions = store_project:versions(State),
  Value = term_to_binary(Versions),
  {Value, Req, State}.
