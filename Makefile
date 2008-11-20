PDFLATEX := pdflatex -shell-escape
BIBTEX := bibtex8 -B
GNUPLOT := gnuplot
MZSCHEME := mzscheme

DOCNAME := paper

define get-field
$(shell echo $1 | cut -d- -f $2)
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

%-contours.gp: %-contours.setup \
               make-contours-gp.sh \
               contours.tpl.gp contour-level.tpl.gp
	${SHELL} make-contours-gp.sh $*-contours.setup > $@

%-contours.tkz.tex: %-contours.gp \
                    plot-contours.sh \
                    contour-path.tpl.tkz.tex
	${SHELL} plot-contours.sh $* > $@

%-trace: test-functions.ss runner.ss
	${MZSCHEME} runner.ss $* > $@

%-trace.tkz.tex: %-trace \
                 plot-trace.sh \
                 trace-path.tpl.tkz.tex
	${SHELL} plot-trace.sh $(call get-function,$*) > $@

doc: ${DOCNAME}.pdf

clean:
	@rm -frv `hg status --unknown --no-status`