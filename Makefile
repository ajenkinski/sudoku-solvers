#PROFILE=1

ifdef PROFILE
GHC=ghc
GHCFLAGS=-O -prof -auto-all
else
GHC=ghc
GHCFLAGS=-O
endif

GHCFLAGS+=-threaded

PROGRAMS=LogicSolver AlgoXSolver SudokuTestQC SudokuTest

MAKEPROGRAM=$(GHC) --make $(GHCFLAGS) -o $@ $^

.PHONY: $(PROGRAMS)

default: LogicSolver

define PROGRAM_template
$(1): $(1).hs
	$(GHC) --make $(GHCFLAGS) -o $$@ $$^
endef

$(foreach prog,$(PROGRAMS),$(eval $(call PROGRAM_template,$(prog))))

runtests: SudokuTestQC
	./SudokuTestQC

clean:
	rm -f *.o *.hi $(PROGRAMS)
