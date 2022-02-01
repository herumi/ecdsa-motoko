all: test.wasm

.SUFFIXES: .wasm .mo

test: test.wasm
	wasmtime $<

test-hmac: hmac-test.wasm
	wasmtime $<

TIME=time

%.wasm: %.mo $(shell echo ../src/*.mo)
	$(shell vessel bin)/moc $(shell vessel sources) --release -wasi-system-api -o $@ $<

bench-gen10.mo: bench-gen.tmpl
	sed 's/@@@/10/' < bench-gen.tmpl > bench-gen10.mo

bench-gen20.mo: bench-gen.tmpl
	sed 's/@@@/20/' < bench-gen.tmpl > bench-gen20.mo

bench-gen10: bench-gen10.wasm
	$(TIME) wasmtime $<
bench-gen20: bench-gen20.wasm
	$(TIME) wasmtime $<

bench-gen:
	$(MAKE) bench-gen10
	$(MAKE) bench-gen20

bench-sign10.mo: bench-sign.tmpl
	sed 's/@@@/10/' < bench-sign.tmpl > bench-sign10.mo

bench-sign20.mo: bench-sign.tmpl
	sed 's/@@@/20/' < bench-sign.tmpl > bench-sign20.mo

bench-sign10: bench-sign10.wasm
	$(TIME) wasmtime $<
bench-sign20: bench-sign20.wasm
	$(TIME) wasmtime $<

bench-sign:
	$(MAKE) bench-sign10
	$(MAKE) bench-sign20

clean:
	rm -rf *.wasm

.PHONY: test
