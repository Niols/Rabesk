
all:
	ocamlbuild -I src/ -lib graphics src/main.byte
	mkdir -p bin
	mv main.byte bin/rabesk

clean:
	ocamlbuild -clean
	rm -rf bin
	find -type f -iregex '.*[~#]' -exec rm -f '{}' \;
