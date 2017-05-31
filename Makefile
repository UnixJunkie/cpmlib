.PHONY: build config clean edit install uninstall reinstall tests

build: config
	obuild build

config: cpm.obuild
	obuild configure

clean:
	obuild clean

edit:
	emacs src/*.ml &

install:
	opam remove cpm
	opam pin -y add cpm ${PWD}

uninstall:
	opam remove cpm

reinstall: uninstall install

# unit tests
tests:
	./test
