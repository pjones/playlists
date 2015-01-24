################################################################################
.PHONEY: all

################################################################################
# Set up the default target.
all::

################################################################################
# Ask `git' to update the submodule and make haskell.mk available.
util/haskell.mk:
	git submodule update --init

################################################################################
# From util/haskell.mk (git submodule update --init)
CABAL_FLAGS = --enable-tests -fmaintainer
include util/haskell.mk

################################################################################
TOOL = $(SANDBOX)/bin/playlist
RTS_OPS = +RTS -sstderr -hc -pa -xc
PROF_LOOPS = 1000

################################################################################
clean::
	rm -rf dist
	rm -f playlist-pls.*
	rm -f playlist-m3u.*

################################################################################
prof::
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
