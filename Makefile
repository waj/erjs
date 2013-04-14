all: compile

compile:
	./rebar compile skip_deps=true

eunit:
	./rebar eunit skip_deps=true

deps:
	./rebar get-deps
	./rebar compile

run:
	erl -pa ebin/ deps/*/ebin
