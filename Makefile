PREFIX := /usr/local
BINDIR := /bin
BINNAME := tangle

GHCFLAGS := -Wall -Werror -O2

build:
	ghc --make Main.hs -o ${BINNAME} ${GHCFLAGS}

clean:
	rm -f *.{hi,o} Trie/*.{hi,o}

#test:
#	runhaskell Test.hs

install: build
	mkdir -p ${DESTDIR}${PREFIX}${BINDIR}
	mv tangle ${DESTDIR}${PREFIX}${BINDIR}
	chmod 755 ${DESTDIR}${PREFIX}${BINDIR}/${BINNAME}

uninstall:
	rm -f ${DESTDIR}${PREFIX}${BINDIR}/${BINNAME}

.PHONY: build clean test install uninstall

