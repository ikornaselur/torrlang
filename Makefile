.SUFFIXES: .erl .beam .yrl

.erl.beam:
	@erlc -o ebin/ -W $<

.yrl.erl:
	@erlc -W ebin/ $<

ERL = erl -boot start_clean

MODS = src/torrlang \
			 src/bencoding src/bencoding_tests \
			 src/trackers \
			 src/urllib src/urllib_tests
all: compile

# Compile all erlang modules
compile: ${MODS:%=%.beam}

# Run a specific problem
run: compile
	@${ERL} -pa ebin -noshell -s torrlang test -s init stop

test: compile
	@${ERL} -pa ebin -noshell -eval 'eunit:test("ebin", [verbose])' -s init stop

# Run the dialyzer on all .erl files
dialyzer:
	@dialyzer src/*.erl

# Cleanup
clean:
	rm -rf ebin/*.beam erl_crash.dump
