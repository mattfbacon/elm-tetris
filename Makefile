SRC := src
DIST := dist

ELM_SRC := $(SRC)/elm
ELM_FILES := $(shell find $(ELM_SRC)/ -name "*.elm" -type f)

LESS_SRC := $(SRC)/less
LESS_FILES := $(shell find $(LESS_SRC)/ -name "*.less" -type f)
LESS_OUT := $(patsubst $(LESS_SRC)/%.less,$(DIST)/%.css,$(LESS_FILES))

STATIC_SRC := $(SRC)/static
STATIC_FILES := $(shell find $(STATIC_SRC)/ -type f)
STATIC_OUT := $(patsubst $(STATIC_SRC)/%,$(DIST)/%,$(STATIC_FILES))

.PHONY: build view makedir
.DEFAULT_GOAL := build

makedir:
	mkdir -p $(DIST)/

$(DIST)/index.js: $(ELM_FILES)
	elm make --debug --output=$@ $(ELM_SRC)/Index.elm
	# uglifyjs $@ --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $@

$(DIST)/%.css: $(LESS_SRC)/%.less
	lessc -x --math=parens $< $@

$(DIST)/%: $(STATIC_SRC)/%
	cp $< $@

build: $(DIST)/index.js $(STATIC_OUT) $(LESS_OUT) | makedir

view: build
	live-server $(DIST)/
