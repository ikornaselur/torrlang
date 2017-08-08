REBAR := $(shell command -v rebar3 2> /dev/null)


.PHONY: test

# Install tools required for the project
env:
	brew install rebar3

rebar_check:
ifndef REBAR
	$(error "rebar3 not installed. Run the 'env' target to install")
endif

# Run all EUnit tests
tests: rebar_check
	@rebar3 eunit
test: tests

# Run the dialyzer
dialyzer: rebar_check
	@rebar3 dialyzer
lint: dialyzer

# Clean all .beam files
clean: rebar_check
	@rebar3 clean


# Run unit tests with coverage
cover: rebar_check
	@rebar3 eunit --cover > /dev/null
	@rebar3 cover
