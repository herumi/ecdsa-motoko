.PHONY: check docs test

check:
	find src -type f -name '*.mo' -print0 | xargs -0 $(shell vessel bin)/moc $(shell vessel sources) --check

all: check-strict docs test

check-strict:
	find src -type f -name '*.mo' -print0 | xargs -0 $(shell vessel bin)/moc $(shell vessel sources) -Werror --check
docs:
	$(shell vessel bin)/mo-doc
test:
	$(MAKE) -C test test

bench-gen:
	$(MAKE) -C test bench-gen

bench-sign:
	$(MAKE) -C test bench-sign

bench:
	$(MAKE) bench-gen
	$(MAKE) bench-sign

clean:
	$(MAKE) -C test clean
