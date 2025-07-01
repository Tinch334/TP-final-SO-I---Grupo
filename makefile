ERLC = erlc
ERL_FILES = downloader.erl node.erl server.erl udp_gen.erl file_gen.erl utils.erl
HRL_FILES = config.hrl
BEAM_FILES = $(ERL_FILES:.erl=.beam)

.PHONY: all clean

all: $(BEAM_FILES)

%.beam: %.erl $(HRL_FILES)
	$(ERLC) +debug_info -Wall $<

clean:
	rm -f *.beam

# erl -noshell -s node init -s init stop