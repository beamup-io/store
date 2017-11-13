-module(store_http_handler_release_single).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

-export([from_gzip/2]).
-export([to_gzip/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"gzip">>, []}, from_gzip}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"gzip">>, []}, to_gzip}], Req, State}.

resource_exists(Req, _State) ->
  ProjectName = cowboy_req:binding(project_name, Req),
  TargetArch = cowboy_req:binding(target_arch, Req),
  Branch = cowboy_req:binding(branch, Req),
  Version = cowboy_req:binding(version, Req),
  State = store_release:new(#{project_name => ProjectName,
                              target_arch => TargetArch,
                              branch => Branch,
                              version => Version}),
  {store_release:exists(State), Req, State}.

from_gzip(Req, State) ->
  {ok, Binary} = receive_stream(Req, <<>>),
  ok = store_release:put(Binary, State),
  {true, Req, State}.

receive_stream(Req, Acc) ->
  case cowboy_req:read_body(Req) of
    {ok, Data, _Req} ->
      Acc2 = <<Acc/binary, Data/binary>>,
      {ok, Acc2};
    {more, Data, Req} ->
      Acc2 = <<Acc/binary, Data/binary>>,
      receive_stream(Req, Acc2)
  end.

to_gzip(Req, State) ->
  Path = store_release:release_path(State),
  Length = filelib:file_size(Path),
  {{sendfile, 0, Length, Path}, Req, State}.
