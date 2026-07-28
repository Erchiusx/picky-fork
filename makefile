export SHELL=/bin/zsh
.DEFAULT_GOAL := dist

.PHONY: dist run clean wasm lint


define WASM_CABAL
	cabal --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs $(1) $(2) $(3)
endef

define WIZER
	env -i GHCRTS=-H64m $$HOME/.ghc-wasm/wasmtime/bin/wizer \
		--allow-wasi \
		--wasm-bulk-memory true \
		--inherit-env true \
		--init-func _initialize \
		-o $(2) \
		$(1) && \
	$$HOME/.ghc-wasm/binaryen/bin/wasm-opt $(2) -o $(2); \
	$$HOME/.ghc-wasm/wasmtime/bin/wasm-tools strip -o $(2) $(2);
endef


define NVM
	NVM_DIR="$$HOME/.nvm" \
	[ -s "$$NVM_DIR/nvm.sh" ] && \. "$$NVM_DIR/nvm.sh" \
	&& nvm $(1) $(2) $(3)
endef


APP_DIST=dist
WASM_DIST=release
SERVER_TARGET=dust
WASM_TARGET=dustify
SRC=src

dist:
	cabal build exe:$(SERVER_TARGET)
	mkdir -p $(APP_DIST)
	install -m 755 $$(cabal list-bin exe:$(SERVER_TARGET)) $(APP_DIST)/$(SERVER_TARGET)
	rm -rf $(APP_DIST)/assets $(APP_DIST)/client
	cp -a assets $(APP_DIST)/assets
	cp -a client $(APP_DIST)/client

run: dist
	cd $(APP_DIST) && ./$(SERVER_TARGET)

clean:
	rm -rf $(APP_DIST)

wasm: wasm-distpath $(WASM_DIST)/ffi.mjs $(WASM_DIST)/$(WASM_TARGET).wasm

main: wasm

$(WASM_DIST)/ffi.mjs: $(WASM_DIST)/$(WASM_TARGET).wasm
	$(call NVM, run, stable) $$(realpath $$(wasm32-wasi-ghc --print-libdir)/post-link.mjs) -i $< -o $@

wasm-distpath:
	mkdir -p $(WASM_DIST)

$(WASM_DIST)/$(WASM_TARGET).wasm: $(SRC)/Dustify.hs
	$(call WASM_CABAL, build, $(notdir $(basename $@)))
	cp $$($(call WASM_CABAL, list-bin, $(notdir $(basename $@)))) $@

lint:
	fourmolu --config ./fourmolu.yaml -i src/ lib/
