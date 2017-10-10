.PHONY: dev test

dev:
	./rebar3 compile && ./rebar3 shell

test:
	./rebar3 ct
