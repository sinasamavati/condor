PROJECT = condor
CT_SUITES = condor

EBIN = $(CURDIR)/ebin
TEST_DIR = $(CURDIR)/test
PLT_FILE = $(CURDIR)/.$(PROJECT).plt

ERLC_OPTS += +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard

V ?= 0

erlc_verbose_0 = @echo "  ERLC  " $(?F);
erlc_verbose = $(erlc_verbose_$(V))

gen_verbose_0 = @echo "  GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

CT_OPTS ?=
CT_RUN = ct_run \
	-no_auto_compile \
	-noshell \
	-pa $(EBIN) \
	-dir $(TEST_DIR) \
	-logdir logs \
	$(CT_OPTS)

CT_SUITES ?=

PLT_APPS ?=
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions

.PHONY: all app shell test dialyze clean distclean

all: app

# ------------------------------------------------------------------------------
# build application
# ------------------------------------------------------------------------------
app: ebin/ ebin/$(PROJECT).app

ebin/: $(wildcard src/*.erl)
	@mkdir -p $(EBIN)
	$(erlc_verbose) erlc -v -o ebin $(ERLC_OPTS) $?

ebin/%.app: src/%.app.src
	cp $< $@

# ------------------------------------------------------------------------------
# run erlang shell
# ------------------------------------------------------------------------------
shell:
	erl -pa $(EBIN)

# ------------------------------------------------------------------------------
# run tests
# ------------------------------------------------------------------------------
test: ERLC_OPTS += -DTEST=1 +export_all
test: clean app
	$(gen_verbose) erlc -v -o test $(ERLC_OPTS) \
		$(wildcard test/*.erl test/*/*.erl) -pa ebin
	@mkdir -p logs
	@$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES))
	@rm -f test/*.beam

# ------------------------------------------------------------------------------
# dialyzer
# ------------------------------------------------------------------------------
dialyze: $(PLT_FILE)
	@dialyzer +S 8 --src src --plt $(PLT_FILE) --no_native $(DIALYZER_OPTS)

$(PLT_FILE):
	$(gen_verbose) dialyzer +S 8 --build_plt --output_plt $@ \
		--apps erts kernel stdlib $(PLT_APPS)

# ------------------------------------------------------------------------------
# clean application
# ------------------------------------------------------------------------------
clean:
	rm -rf $(EBIN) $(CURDIR)/erl_crash.dump

# ------------------------------------------------------------------------------
# clean project
# ------------------------------------------------------------------------------
distclean: clean
	rm -rf $(CURDIR)/logs $(CURDIR)/*.beam $(PLT_FILE)
