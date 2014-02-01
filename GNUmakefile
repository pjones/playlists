################################################################################
# A stupid Makefile that doesn't do much.
.PHONEY: all test clean prof

################################################################################
SANDBOX = .cabal-sandbox
TOOL = $(SANDBOX)/bin/playlist
RTS_OPS = +RTS -sstderr -hc -pa -xc
PROF_LOOPS = 1000

################################################################################
all: $(SANDBOX)
	cabal install

################################################################################
test::
	cabal install --enable-tests

################################################################################
clean::
	rm -rf dist
	rm -f playlist-pls.*
	rm -f playlist-m3u.*

################################################################################
prof:
	rm -rf dist
	cabal install --enable-library-profiling \
	  --enable-executable-profiling \
	  --flags="profiling" --disable-executable-stripping
	(for i in `seq $(PROF_LOOPS)`; do cat test/sa.pls; done) |\
	  $(TOOL) convert --from PLS --to PLS \
	    $(RTS_OPS) 2> playlist-pls.stats > /dev/null
	mv playlist.prof playlist-pls.prof
	mv playlist.hp playlist-pls.hp
	hp2ps -M -b -c playlist-pls.hp
	ps2pdf playlist-pls.ps
	(for i in `seq $(PROF_LOOPS)`; do cat test/hp.m3u; done) |\
	  $(TOOL) convert --from M3U --to M3U \
	    $(RTS_OPS) 2> playlist-m3u.stats > /dev/null
	mv playlist.prof playlist-m3u.prof
	mv playlist.hp playlist-m3u.hp
	hp2ps -M -b -c playlist-m3u.hp
	ps2pdf playlist-m3u.ps

################################################################################
$(SANDBOX):
	cabal sandbox init
