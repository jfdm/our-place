# A Makefile

PANDOC   := $(shell which pandoc)
OURPLACE := $(shell which our-place)
OURQUIZ  := $(shell which our-quiz)

CONTENT := $(shell find content/ -type f -iname "*.md")
QUIZZES := $(foreach q,$(subst yaml,html,$(shell find quiz/ -type f -iname "*.yaml")),autogen/$(q))
OUTLINE := $(shell find content/ -type d)
OUTLINE_Q := $(shell find quiz/ -type d)

default:
	@echo "Building The Project"
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
	mkdir --parent $(foreach q,$(OUTLINE_Q),autogen/$(q))

quizzes: setup ${QUIZZES}

autogen/nav.html: toc.yaml setup
	echo "" \
	  | pandoc --template templates/nav-template.html \
		   --metadata-file $< \
		   --to=html5 \
		   --from=markdown \
		   --output=$@

autogen/quiz/%.html: quiz/%.yaml setup
	${OURQUIZ} $< $@


build rebuild check watch: autogen/nav.html $(CONTENT) $(QUIZZES)
	${OURPLACE} $@

.PHONY: clean
clean:
	${OURPLACE} $@

.PHONY: clobber
clobber: clean
	${RM} -r autogen

# -- [ EOF ]
