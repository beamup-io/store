-module(store_project).

-export([new/1, dir/1, exists/1, versions/1]).

new(Project) ->
  validate(Project).

dir(Project) ->
  BaseDir = <<"/tmp/beamup/store/projects/">>,
  Name = maps:get(project_name, Project),
  Architecture = maps:get(architecture, Project),
  Branch = maps:get(branch, Project),
  Folder = <<Name/binary, $/, Architecture/binary, $/, Branch/binary>>,
  <<BaseDir/binary, Folder/binary, $/>>.

exists(Project) ->
  filelib:is_file(dir(Project)).

versions(Release) ->
  Branch = maps:get(branch, Release),
  N = string:length(Branch),
  {ok, Filenames} = file:list_dir(dir(Release)),
  io:format("Fn: ~p~n", [Filenames]),
  lists:filtermap(fun(F) ->
    io:format("F: ~p~n", [F]),
    Filename = filename:basename(list_to_binary(F), ".tar.gz"),
    case Filename of
      <<Branch:N/binary, $-, Version/binary>> ->
        {true, Version};
      _ -> false
    end
  end, Filenames).

% Private

validate(P = #{project_name := ProjectName,
               architecture := Architecture,
               branch := Branch}) ->
  true = valid_name(ProjectName),
  true = valid_name(Architecture),
  true = valid_name(Branch),
  P.

valid_name(<<>>) -> true;
valid_name(<<$., _T/binary>>) -> false;
valid_name(<<$/, _T/binary>>) -> false;
valid_name(<<_Char, T/binary>>) -> valid_name(T).
