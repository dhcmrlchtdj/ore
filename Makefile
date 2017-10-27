OCB_FLAGS := \
	-tag 'color(always)' \
	-tags safe_string,strict_sequence,strict_formats,short_paths,keep_locs \
	-use-ocamlfind -pkgs 'str' \
	-tags 'warn(+a-4-42),warn_error(-a+31)'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

main: $(mlis)
	@$(OCB) src/main.byte

$(mlis):
	@$(OCB) $@.inferred.mli

clean:
	@ocamlbuild -clean

.PHONY: main clean $(mlis)
