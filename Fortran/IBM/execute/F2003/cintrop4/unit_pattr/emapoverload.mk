obj = emapoverload-c.o emapoverload-f.o pf.o

all: over
	./$?
	[ "$(PDF_OPT)" != -qpdf2 ]  ||	\
		[ "$(TRUN_SAVE)" = yes ]  ||  rm -f $(PDF_FILE)

#
#  Defect 291139:  (Dummy) Target added for .pdf handling
#
tc-emapoverload-c:

tc-emapoverload-f: tc-emapoverload

tc-emapoverload: cl-emapoverload all  # CLean then re-do TestCase
cl-emapoverload:; rm -f $(obj) emapoverload-c.lst emapoverload-f.lst over

over: $(obj)  # implicit command isn't invoked.  Need explicit:
	$(LINK.o) $+  -o $@
