PREFIX := /usr/local
BINDIR := /bin
BINNAME := tangle

GHCFLAGS = --make Main.hs -o ${BINNAME} -dynamic -isrc

build:
	ghc ${GHCFLAGS}

clean:
	rm -f ${BINNAME} *.hi *.o

test:
	runhaskell Test.hs

install: build
	mkdir -p ${DESTDIR}${PREFIX}${BINDIR}
	cp tangle ${DESTDIR}${PREFIX}${BINDIR}
	chmod 755 ${DESTDIR}${PREFIX}${BINDIR}/${BINNAME}

uninstall:
	rm -f ${DESTDIR}${PREFIX}${BINDIR}/${BINNAME}

.PHONY: build clean test install uninstall

