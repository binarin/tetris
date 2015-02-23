PROJECT = tetris
DEPS = cowboy bullet sync lager jiffy erlydtl gproc
include erlang.mk

shell: SHELL_OPTS=-name tetris@localhost -eval 'application:ensure_all_started(tetris).' -eval 'sync:go().'
