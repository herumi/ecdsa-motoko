.PHONY: check docs test

check:
	find src -type f -name '*.mo' -print0 | xargs -0 $(shell vessel bin)/moc $(shell vessel sources) --check

all: check-strict docs test src/fp.mo

check-strict:
	find src -type f -name '*.mo' -print0 | xargs -0 $(shell vessel bin)/moc $(shell vessel sources) -Werror --check
docs:
	$(shell vessel bin)/mo-doc
test: src/fp.mo
	$(MAKE) -C test test

bench-gen:
	$(MAKE) -C test bench-gen

bench-sign:
	$(MAKE) -C test bench-sign

bench-verify:
	$(MAKE) -C test bench-verify

bench:
	$(MAKE) bench-gen
	$(MAKE) bench-sign
	$(MAKE) bench-verify

test-hmac:
	$(MAKE) -C test test-hmac

src/fp.mo: src/gen.py
	python3 src/gen.py > $@
test-fp: src/fp.mo
	$(MAKE) -C test test-fp

clean:
	$(MAKE) -C test clean
