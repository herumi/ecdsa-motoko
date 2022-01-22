default:
	$(shell vessel bin)/moc $(shell vessel sources) -wasi-system-api -o Test.wasm Test.mo && wasmtime Test.wasm
	rm -f Test.wasm

bench-gen.wasm: bench-gen.mo
	$(shell vessel bin)/moc $(shell vessel sources) --release -wasi-system-api -o bench-gen.wasm bench-gen.mo

bench-gen: bench-gen.wasm
	time -f %U wasmtime bench-gen.wasm
	time -f %U wasmtime bench-gen.wasm
	time -f %U wasmtime bench-gen.wasm
#	rm -f bench-gen.wasm
