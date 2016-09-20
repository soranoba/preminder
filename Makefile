APP := preminder
TAG := $(shell git describe --always --tags)

all: compile eunit xref dialyze

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

release:
	@./rebar3 as prod release

build: release
	docker build --rm --tag=soranoba/$(APP):$(TAG) .
	docker tag -f soranoba/$(APP):$(TAG) soranoba/$(APP):latest

start:
	@if [ ! -f sys.config ]; then \
		>&2 echo "sys.config is not found. please see sys.config.template"; \
		exit 1; \
	fi
	@./rebar3 as dev shell --sname $(APP) --config sys.config

dialyze:
	@./rebar3 dialyzer
