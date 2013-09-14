PREFIX := /usr/local
BINDIR := /bin
BINNAME := tangle

GHCFLAGS := -Wall -Werror -O2

build: test
	@ghc --make Main.hs -o ${BINNAME} ${GHCFLAGS}

clean:
	@rm -f *.{hi,o}

test:
	@runhaskell tests.hs

install: build
	@mkdir -p ${DESTDIR}${PREFIX}${BINDIR}
	@mv tangle ${DESTDIR}${PREFIX}${BINDIR}
	@chmod 755 ${DESTDIR}${PREFIX}${BINDIR}/${BINNAME}

uninstall:
	@rm -f ${DESTDIR}${PREFIX}${BINDIR}/${BINNAME}

.PHONY: build clean test install uninstall

