HC=ghc
SOURCES=Main.hs
PACKAGE=hw1.zip

.PHONY: pack all run clean

all: parser

run:
	./parser

clean:
	rm -rf /*.o /*.hi
	rm -f parser

parser: $(SOURCES)
	$(HC) -i -tmpdir . ./Main.hs -o parser
pack:
	zip $(PACKAGE) -r Makefile Main.hs src
