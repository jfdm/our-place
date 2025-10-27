# A Makefile

CONTENT := $(shell find content/ -type f -iname "*.md")
QUIZZES := $(subst quiz,autogen,$(subst yaml,html,$(shell find quiz/ -type f -iname "*.yaml")))
OUTLINE := $(shell find content/ -type d)

default:
	@echo "Building Our Place"
	@echo ""
	@echo "setup   generate directories for dumping quizzes."
	@echo "quizzes generate html quizzes from yaml descriptions in quiz"
	@echo "        quizzes are stored in autogen"
	@echo "clobber run clean and remove autogen"
	@echo "X       commands supported from hakyll"
	@echo "        - build"
	@echo "        - check"
	@echo "        - clean"
	@echo "        - rebuild"
	@echo "        - watch"

.PHONY: setup
setup:
	mkdir --parent $(subst content,autogen,$(OUTLINE))

quizzes: setup ${QUIZZES}

autogen/%.html: quiz/%.yaml setup
	cabal run --builddir _dist our-quiz -- $< $@


build check watch: $(CONTENT) $(QUIZZES)
	cabal run our-place $@

.PHONY: clean rebuild
clean rebuld:
	cabal run our-place $@

.PHONY: clobber
clobber: clean
	${RM} -r autogen


# -- [ EOF ]
