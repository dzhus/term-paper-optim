PDFLATEX := pdflatex -shell-escape
BIBTEX := bibtex8 -B
GNUPLOT := gnuplot
MZSCHEME := mzscheme

DOCNAME := paper

define get-target-function
$(shell echo $1 | cut -d- -f 1)
endef

define get-target-method
$(shell echo $1 | cut -d- -f 2)
endef

.SECONDEXPANSION:

.PHONY: doc clean

.PRECIOUS: %-trace

include ${DOCNAME}-deps.mk

${DOCNAME}-deps.mk: ${DOCNAME}.tex
	texdepend -o $@ -print=if $<

${DOCNAME}.aux: ${INCLUDES} ${DOCNAME}.tex ${DOCNAME}.bib
	${PDFLATEX} ${DOCNAME}
	${BIBTEX} ${DOCNAME}

${DOCNAME}.pdf: ${DOCNAME}.aux
	${PDFLATEX} ${DOCNAME}
	${PDFLATEX} ${DOCNAME}

%.tkz: %.tkz.tex

%-contours.tkz.tex: %.gp \
                plot-contours.sh \
                contour-path.tpl.tkz.tex
	${SHELL} plot-contours.sh $* > $@

%-trace: test-functions.ss runner.ss
	${MZSCHEME} runner.ss $* > $@

%-trace.tkz.tex: %-trace \
                 plot-trace.sh \
                 trace-path.tpl.tkz.tex
	${SHELL} plot-trace.sh $(call get-target-function,$*) > $@

doc: ${DOCNAME}.pdf

clean:
	@rm -frv `hg status --unknown --no-status`