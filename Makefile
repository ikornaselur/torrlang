.SUFFIXES: .erl .beam .yrl

.erl.beam:
	@erlc -W $<

.yrl.erl:
	@erlc -W $<

ERL = erl -boot start_clean

MODS = torrlang bencoding test

all: compile

# Compile all erlang modules
compile: ${MODS:%=%.beam}

# Run a specific problem
run: compile
	@${ERL} -noshell -s torrlang test -s init stop

test: compile
	@${ERL} -noshell -s test test -s init stop

# Run the dialyzer on all .erl files
dialyzer:
	@dialyzer *.erl

# Cleanup
clean:
	rm -rf *.beam erl_crash.dump
