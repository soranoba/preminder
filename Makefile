APP := preminder

.PHONY: ct
all: compile eunit ct xref dialyze edoc

compile:
	@./rebar3 as dev compile

xref:
	@./rebar3 xref

clean:
	@./rebar3 clean

ct:
	@./rebar3 ct

cover:
	@./rebar3 cover

eunit:
	@./rebar3 eunit

edoc:
	@./rebar3 as dev edoc

release:
	@./rebar3 as prod release

build: release
	@docker build --rm --tag=$(APP):$(shll git describe --always --tags --long) .

start:
	@if [ ! -f sys.config ]; then \
		>&2 echo "sys.config is not found. please see sys.config.template"; \
		exit 1; \
	fi
	@./rebar3 as dev shell --sname $(APP) --config sys.config

dialyze:
	@./rebar3 dialyzer
