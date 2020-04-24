:- module(make, [run/0]).

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(prolog_pack), []).

run :-
  absolute_file_name(., Dir, [file_type(directory)]),
  absolute_file_name('Makefile', _, [access(read),relative_to(Dir)]),
  prolog_pack:build_environment(Env),
  catch(
    prolog_pack:run_process(path(make), [distclean], [directory(Dir),env(Env)]),
    E,
    print_message(warning, E)
  ),
  prolog_pack:setup_path,
  prolog_pack:save_build_environment(Dir),
  absolute_file_name('buildenv.sh', File, [access(read),relative_to(Dir)]),
  process_create(path(sh), [file(File)], []),
  prolog_pack:configure_foreign(Dir, []),
  prolog_pack:make_foreign(Dir, []).
