#Vaibhav Bhandari 6th Nov
# $Id: Makefile,v 1.16 2003/06/11 13:14:27 vaibhav Exp $
OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDEP = ocamldep
INCLUDES= -verbose str.cma
OCAMLFLAGS=$(INCLUDES)
OCAMLLEXFLAGS=
OCAMLYACCFLAGS=
DOCTOOL = ocamldoc
DOCDIR = mv2rm_doc

FILES= lexer parser main ass codegen ast
#Order below matters for symbol resolution
OBJS= ast.cmo ass.cmo codegen.cmo parser.cmo lexer.cmo main.cmo 
BIN = mv2rm
TEST= ../test/arbiter.mv

MAIN_FILE = $(DOCDIR)
WWW_FILE_BAKUP = $(MAIN_FILE).bak
WWWDIR=/projects/bubble/dvlab

#Rules 
all: $(OBJS)
	$(OCAMLC) -o $(BIN) $(OCAMLFLAGS) $(OBJS)

clean:
	rm -f *.rm
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.cm[io]

htmldoc:
	$(DOCTOOL) -html -d $(DOCDIR) *.ml

webput:
	mv $(WWWDIR)/$(MAIN_FILE) $(WWWDIR)/$(WWW_FILE_BAKUP)
	cp -r $(DOCDIR) $(WWWDIR)
	chmod -R a+rx $(WWWDIR)/$(MAIN_FILE)

neat:
	rm *~

test: all
#	clear
	$(BIN) -o test.rm $(TEST)

update: 
	scp -r vaibhav@kala.cse.ucsc.edu:/projects/bubble/people/vaibhav/blifmv2rm/* .

copy:
	make clean
	scp -r * vaibhav@kala.cse.ucsc.edu:/projects/bubble/people/vaibhav/blifmv2rm/

#Common Rules
.SUFFIXES: .ml .mli .cmo .cmi .mly .mll

.mll.ml:
	$(OCAMLLEX) $(OCAMLLEXFLAGS) $<	
.mly.ml:
	$(OCAMLYACC) $(OCAMLYACCFLAGS) $<		
.ml.cmo:		
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<	

#Dependencies
.depend: parser.ml lexer.ml
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend	

