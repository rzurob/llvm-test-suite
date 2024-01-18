common_obj = emapfn-c.o emapfn-f.o pf.o

all: run-f run-c

run-f: emapfn-fmain
	./$?
	[ "$(PDF_OPT)" != -qpdf2 ]  ||	\
		[ "$(TRUN_SAVE)" = yes ]  ||  rm -f $(PDF_FILE)

run-c: emapfn-cmain
	./$?
	[ "$(PDF_OPT)" != -qpdf2 ]  ||	\
		[ "$(TRUN_SAVE)" = yes ]  ||  rm -f $(PDF_FILE)

tc-emapfn: cl-emapfn all

tc-emapfn-f: cl-emapfn run-f

tc-emapfn-c: cl-emapfn run-c

cl-emapfn:; rm -f emapfn-fmain emapfn-cmain \
		emapfn-cmain.o emapfn-fmain.o $(common_obj) emapfn.mod \
		emapfn-f.lst emapfn-fmain.lst


emapfn-fmain: emapfn-fmain.o $(common_obj)
	$(LINK.f) -o $@ $< $(common_obj)


emapfn-cmain: emapfn-cmain.o $(common_obj)
	$(LINK.f) -o $@ $< $(common_obj)

#
#  Defect 291139:  Target added to prevent the second (.c Main) build
#  from supressing error messages (if any) via $SILENT_BUILD.
#
emapfn-cmain.o: $(TR_SRC)/emapfn-cmain.c
	$(CC) $(CFLAGS) -c $<
