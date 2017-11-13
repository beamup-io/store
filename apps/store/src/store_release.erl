-module(store_release).

-export([new/1, release_path/1, put/2, exists/1]).

new(Release) ->
  validate(Release).

release_path(Release) ->
  ProjectDir = store_project:dir(Release),
  Filename = file_name(Release),
  <<ProjectDir/binary, Filename/binary>>.

put(File, Release) ->
  filelib:ensure_dir(store_project:dir(Release)),
  file:write_file(release_path(Release), File).

exists(Release) ->
  filelib:is_file(release_path(Release)).

% Private

file_name(Release) ->
  Branch = maps:get(branch, Release),
  Version = maps:get(version, Release),
  Extension = <<".tar.gz">>,
  <<Branch/binary, $-, Version/binary, Extension/binary>>.

validate(R = #{version := Version}) ->
  true = valid_name(Version),
  Project = store_project:new(R),
  Release = maps:put(version, Version, Project),
  Release.

valid_name(<<>>) -> true;
valid_name(<<$., _T/binary>>) -> false;
valid_name(<<$/, _T/binary>>) -> false;
valid_name(<<_Char, T/binary>>) -> valid_name(T).
