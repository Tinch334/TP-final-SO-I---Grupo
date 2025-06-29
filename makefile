ERLC = erlc
ERL_FILES = downloader.erl nodo.erl server.erl udp_gen.erl file_gen.erl utils.erl
HRL_FILES = config.hrl gen_header.hrl
BEAM_FILES = $(ERL_FILES:.erl=.beam)

.PHONY: all clean

all: $(BEAM_FILES)

%.beam: %.erl $(HRL_FILES)
	$(ERLC) +debug_info -Wall $<

clean:
	rm -f *.beam

# erl -noshell -s nodo init -s init stop