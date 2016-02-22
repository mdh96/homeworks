CABAL ?= cabal
GHC ?= ghc

.PHONY : all
all : while

.PHONY : create-sandbox
create-sandbox : ~/.cabal/config
	$(CABAL) sandbox init
	$(CABAL) install --dependencies-only

.PHONY : clean
clean :
	$(CABAL) clean
	rm -f test while

.PHONY : dist-clean
dist-clean : clean
	rm -rf cabal.sandbox.config .cabal-sandbox

.PHONY : run-tests
run-tests : test
	./test

~/.cabal/config :
	$(CABAL) update

cabal.sandbox.config : create-sandbox

dist/build/test/test dist/build/while/while : cabal.sandbox.config
	$(CABAL) build

test : dist/build/test/test
	cp $< $@

while : dist/build/while/while
	cp $< $@
