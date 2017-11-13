-module(store_project).

-export([new/1, dir/1, exists/1, versions/1]).

new(Project) ->
  validate(Project).

dir(Project) ->
  BaseDir = <<"/tmp/beamup/store/projects/">>,
  Name = maps:get(project_name, Project),
  Arch = maps:get(target_arch, Project),
  Branch = maps:get(branch, Project),
  Folder = <<Name/binary, $/, Arch/binary, $/, Branch/binary>>,
  <<BaseDir/binary, Folder/binary, $/>>.

exists(Project) ->
  filelib:is_file(dir(Project)).

versions(Release) ->
  {ok, Filenames} = file:list_dir(dir(Release)),
  [filename_to_vsn(F) || F <- Filenames].

% Private

filename_to_vsn(F) ->
  R = filename:rootname(F, ".tar.gz"),
  Vsn = lists:last(string:tokens(R, "-")),
  list_to_binary(Vsn).

validate(P = #{project_name := ProjectName,
               target_arch := TargetArch,
               branch := Branch}) ->
  true = valid_name(ProjectName),
  true = valid_name(TargetArch),
  true = valid_name(Branch),
  P.

valid_name(<<>>) -> true;
valid_name(<<$., _T/binary>>) -> false;
valid_name(<<$/, _T/binary>>) -> false;
valid_name(<<_Char, T/binary>>) -> valid_name(T).
