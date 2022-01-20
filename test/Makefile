default:
	$(shell vessel bin)/moc $(shell vessel sources) -wasi-system-api -o Test.wasm Test.mo && wasmtime Test.wasm
	rm -f Test.wasm

bench-gen:
	$(shell vessel bin)/moc $(shell vessel sources) --release -wasi-system-api -o bench-gen.wasm bench-gen.mo
	time wasmtime bench-gen.wasm
	time wasmtime bench-gen.wasm
	time wasmtime bench-gen.wasm
#	rm -f bench-gen.wasm
