PDFLATEX := pdflatex -shell-escape -halt-on-error -file-line-error
BIBTEX := bibtex8 -B
GNUPLOT := gnuplot
MZSCHEME := mzscheme

DOCNAME := paper
SOURCES := runner.ss relch.ss gradient-methods.ss shared.ss
TESTFILE := tests.ss

# Select fields 1, 2, 3 etc. from 1_2_3-trace.tkz.tex according to the
# following scheme:
# <TEST-NAME>_<METHOD-NAME>_<X.XX,Y.YY>_<MAX-ITERATIONS>_<PARAMETER>-<something>.tkz.tex
define get-field
$(shell echo $1 | sed -e 's/-\w\.tkz\.tex//' | cut -d_ -f $2)
endef

define get-function
$(call get-field,$1,1)
endef

define get-method
$(call get-field,$1,2)
endef

define get-start-point
$(call get-field,$1,3)
endef

define get-max-iterations
$(call get-field,$1,4)
endef

define get-parameter
$(call get-field,$1,5)
endef

define strip-unmanaged
$(shell bash strip-unmanaged.sh $1)
endef

.SECONDEXPANSION:

.PHONY: doc clean

.PRECIOUS: %-trace

include ${DOCNAME}-deps.mk

doc: ${DOCNAME}.pdf

# Depend only on those of included files which are not autogenerated
${DOCNAME}-deps.mk: ${DOCNAME}.tex $(call strip-unmanaged,${INCLUDES})
	texdepend -o $@ -print=if $<

${DOCNAME}.aux: ${INCLUDES} ${DOCNAME}.tex ${DOCNAME}.bib
	${PDFLATEX} ${DOCNAME}
	${BIBTEX} ${DOCNAME}

${DOCNAME}.pdf: ${DOCNAME}.aux
	${PDFLATEX} ${DOCNAME}
	${PDFLATEX} ${DOCNAME}

%.tkz: %.tkz.tex

%-contours.gp: %-contours.setup \
               make-contours-gp.sh \
               contours.tpl.gp contour-level.tpl.gp
	${SHELL} make-contours-gp.sh $*-contours.setup > $@

%-contours.tkz.tex: %-contours.gp \
                    plot-contours.sh \
                    contour-path.tpl.tkz.tex
	${SHELL} plot-contours.sh $* > $@

%-trace: check
	${MZSCHEME} runner.ss \
		-m "$(call get-method,$*)" \
		-s "$(call get-start-point,$*)" \
		-i "$(call get-max-iterations,$*)" \
		-L "$(call get-parameter,$*)" \
	 $(call get-function,$*) > $@

%-trace.tkz.tex: %-trace \
                 plot-trace.sh \
                 trace-path.tpl.tkz.tex
	${SHELL} plot-trace.sh $* > $@

check: ${SOURCES} ${TESTFILE}
	@${MZSCHEME} ${TESTFILE} | sed -e 's/: */:/'
	@touch check

clean:
	@rm -frv `hg status --unknown --no-status`
