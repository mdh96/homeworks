CABAL ?= cabal
GHC ?= ghc
DOT ?= dot

.PHONY : all
all : fsm

.PHONY : create-sandbox
create-sandbox : ~/.cabal/config
	$(CABAL) sandbox init
	$(CABAL) install --dependencies-only

.PHONY : clean
clean :
	$(CABAL) clean
	rm -f fsm test match match.c *.dot *.pdf

.PHONY : dist-clean
dist-clean : clean
	rm -rf cabal.sandbox.config .cabal-sandbox

.PHONY : run-tests
run-tests : test
	./test

~/.cabal/config :
	$(CABAL) update

cabal.sandbox.config : create-sandbox

.PHONY : figs
figs : epsilon.pdf a.pdf ab.pdf a-or-b.pdf ab-star.pdf

test : dist/build/test/test
	cp $< $@

fsm : dist/build/fsm/fsm
	cp $< $@

dist/build/test/test dist/build/fsm/fsm : cabal.sandbox.config
	$(CABAL) build

match : main.c match.c
	gcc -o $@ $^

%.pdf : %.dot
	$(DOT) -Tpdf -o $@ $< || rm -f $@

%-nfa.dot %-dfa.dot %-min-dfa.dot : %.txt fsm
	./fsm --nfa-dot=$*-nfa.dot --dfa-dot=$*-dfa.dot --min-dfa-dot=$*-min-dfa.dot $(shell cat $<)

epsilon.dot : fsm
	./fsm --nfa-dot=$@ ""

a.dot : fsm
	./fsm --nfa-dot=$@ "a"

ab.dot : fsm
	./fsm --nfa-dot=$@ "ab"

a-or-b.dot : fsm
	./fsm --nfa-dot=$@ "a|b"

ab-star.dot : fsm
	./fsm --nfa-dot=$@ "(ab)*"
