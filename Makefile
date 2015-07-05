SOURCES = operator.ml type.ml syntax.ml parser.mly lexer.mll \
	  gensym.ml knormal.ml env.ml \
	  alpha.ml beta.ml eta.ml assoc.ml elim.ml constf.ml \
	  first.ml \
	  register.ml prealloc.ml alloc.ml \
	  code.ml \
	  main.ml
RESULT = compiler
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
