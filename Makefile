PROJECT = forge
.DEFAULT_GOAL = deps
ERLC_OPTS ?= +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard # +bin_opt_info +warn_missing_spec
build: app
	#erlc -pa ebin test/trecord_test.erl
	@echo "OK"

DEPS = parse_trans merl
dep_merl = https://github.com/jlouis/merl.git
dep_parse_trans = https://github.com/uwiger/parse_trans.git

CT_SUITES = eunit

include erlang.mk
