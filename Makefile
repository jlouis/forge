PROJECT = forge
.DEFAULT_GOAL = deps

DEPS = parse_trans
dep_parse_trans = https://github.com/uwiger/parse_trans.git

include erlang.mk
