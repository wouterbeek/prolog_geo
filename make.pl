:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(prolog_pack), []).

make :-
  absolute_file_name(., Dir, [file_type(directory)]),
  catch(
    make(Dir, [distclean]),
    E,
    print_message(warning, E)
  ),
  post_install_foreign(Dir).

make(Dir, Targets) :-
  directory_file_path(Dir, 'Makefile', File),
  exists_file(File), !,
  prolog_pack:build_environment(Env),
  Options = [directory(Dir),env(Env)],
  forall(
    member(Target, Targets),
    prolog_pack:run_process(path(make), [Target], Options)
  ).
make(_, _).

post_install_foreign(Dir) :-
  prolog_pack:setup_path,
  prolog_pack:save_build_environment(Dir),
  prolog_pack:configure_foreign(Dir, []),
  prolog_pack:make_foreign(Dir, []).
